buildCondition <- function(conditions) {
    condition_texts <- c()
    for (i in 1:nrow(conditions)) {
        text <- (paste(conditions$query_string[i], "~", conditions$value[i]))
        condition_texts <- c(condition_texts, text)
    }
    return(paste(condition_texts, collapse = ", "))
}

dryOrWet <- function(df, mode ) {

    group_vars <- switch(
        mode,
        "by_site_and_period" = c("site_name", "period"),
        "by_site" = "site_name",
        "by_period" = "period"
    )
    
    # case_when_string <- paste( "case_when(", buildCondition(dry_wet_conditions), ")" ) # with feet
    case_when_string <- paste( "case_when(", buildCondition(dry_wet_conditions[-8,]), ")" ) # without feet

    dry_or_wet <- df %>%
        group_by( across(all_of(group_vars)) ) %>%
            mutate(
                state = eval( parse_expr(case_when_string) )
            )

    summary <- dry_or_wet %>%
        summarize(
            .groups = "drop",
            DRY = round( sum(state == -3) + 0.75*sum(state == -2) + 0.5*sum(state == -1), 0 ),
            WET = round( sum(state == 3) + 0.75*2*sum(state == 2) + 0.5*sum(state == 1), 0 ),
            na = sum( state == 0 ),
            total = sum(!is.na(state)),
            ident_rate = paste(round(100*(sum(!is.na(state)) - sum( state == 0 ))/sum(!is.na(state)), 0), "%"),
            dry3 = sum( state == -3 ),
            dry2 = sum( state == -2 ),
            dry1 = sum( state == -1 ),
            wet3 = sum( state == 3 ),
            wet2 = sum( state == 2 ),
            wet1 = sum( state == 1 )
        )

    plot_data <- summary %>%
        pivot_longer( cols = c(DRY, WET), names_to = "condition", values_to = "value" ) %>%
            group_by( across(all_of(group_vars)), condition ) %>%
                summarise( total = sum(value),.groups = 'drop' ) %>%
                    group_by( across(all_of(group_vars)) ) %>%
                        mutate( percentage = total / sum(total) * 100 )

    if(mode != "by_site") {
        plot_data <- plot_data %>%
            mutate( period = factor(period, levels = c("Shang", "Zhou")) )
    }

    facet_formula <- paste( "~", paste( group_vars, collapse = " + " ) )
    
    piecharts <- ggplot(plot_data, aes(y = percentage, fill = condition)) +
    geom_bar(aes(x = factor(1)), stat = "identity", width = 1, colour = "white", linewidth = 1) +
    geom_text(aes(x = factor(1), label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5), size = 4, color = "white") +
    coord_polar("y", start = 0) +
    facet_wrap(as.formula(facet_formula), ncol = 2) +
    theme_void() +
    labs(x = NULL, y = NULL, fill = "Condition", title = sprintf("DRY versus WET by %s", paste(group_vars, collapse = " and "))) +
    scale_fill_manual(values = drywetColors) +
    theme_ipsum(base_family = "opensans") +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 15, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        panel.spacing = unit(2, "lines"),
        plot.title = element_markdown(size = 20, face = "bold", hjust = 0.5)
    )

    stat_data <- summary %>%
        mutate( group = do.call(paste, c(select(., all_of(group_vars)), sep = ", ")) ) %>%
        select( group, DRY, WET )
    data_matrix <- as.matrix( stat_data[, c("DRY", "WET")] )
    rownames(data_matrix) <- stat_data$group
    ftest <- fisher.test(data_matrix, alternative = "two.sided")
    pvalue <- format(ftest$p.value, scientific = FALSE, digits = 3)

     conditions <- dry_wet_conditions %>%
        select(header, help, value)


dryCOLindex <- which(names(summary) == "DRY")
wetCOLindex <- which(names(summary) == "WET")
    
table_sum <- ggtexttable(
    summary,
    rows = NULL,
    theme = ttheme(
        "light",
        padding = unit(c(1.5, 6), "mm")
        )
    )

table_sum <- table_cell_bg(
    table_sum,
    row = 2:(nrow(summary) + 1),
    column = wetCOLindex,
    fill=lighten(unname(drywetColors["WET"]), amount = 0.3),
    color = "white"
    )

table_sum <- table_cell_bg(
    table_sum,
    row = 2:(nrow(summary) + 1),
    column = dryCOLindex,
    fill=lighten(unname(drywetColors["DRY"]), amount = 0.2),
    color = "white"
    )

table_cond <- ggtexttable(
    conditions,
    rows = NULL,
    theme = ttheme(
        "light",
        padding = unit(c(4, 6), "mm")#,
     #   colnames.style = colnames_style(
     #       color = "white",
     #       fill = "#4C72B0",
     #       face = "bold", size = 10),
    #tbody.style = tbody_style(color = "black", fill = c("#EAEAF2", "#FFFFFF"), size = 10)
        )
    )

table_cond <- table_cell_bg(
    table_cond,
    row = 2:4,
    column = 1,
    fill=lighten(unname(drywetColors["DRY"]), amount = 0.2),
    color = "white"
    )

table_cond <- table_cell_bg(
    table_cond,
    row = 5:9,
    column = 1,
    fill=lighten(unname(drywetColors["WET"]), amount = 0.3),
    color = "white"
    )

    annotations <- c(
        annotate(
            "text",
           x=0.5,
           y=-0.3,
            label = "DRY = dry3 + 0.75*dry2 + 0.5*dry1",
            color = "blue",
            size = 4
        ),
        annotate(
            "text",
            x=0.5,
           y=-0.4,
            label = "WET = wet3 + 0.75*wet2 + 0.5*wet1",
            color = "blue",
            size = 4
        ),
        annotate(
            "text",
          x=0.5,
           y=-2,
            label = paste( "Fisher's Exact Test p-value:", pvalue ),
            color = if(pvalue <= 0.05) {"red"} else {"black"},
            size = 5
        )
    )

    design <- c(
        area(1,2,3,2), # piechart
        area(1,1,1,1), # table conditions
        area(2,1,2,1), # annotations
        area(3,1,3,1) # table sum
         
    )

    combined_plot <- 
    piecharts +
    table_cond +
    annotations +
    table_sum +
    plot_layout( design = design, heights=c(3,6,0.1), widths = c(1, 1) ) 
    
    plot_name <- paste0("dry_wet_", paste( group_vars, collapse = "+" ))

    plot_data <- list(name = plot_name, plot = combined_plot, data = dry_or_wet)

    result <- list(plots = plot_data, table = dry_or_wet)
    return(result)
    
   
}