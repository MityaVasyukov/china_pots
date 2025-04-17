## ---- dryorwet ----
dryOrWet <- function(mode) {
    # loadings & settings
        source("buildCondition.r")
        source("load_pckgs.r")
        source("settings.r")
        dependencies <- c("readxl", "dplyr", "tidyr", "ggplot2", "rlang", "hrbrthemes", "ggtext", "ggpubr", "colorspace", "patchwork", "knitr")
        invisible(lapply(dependencies, load_pkg))
        grDevices::windowsFonts(opensans = windowsFont("Open Sans"))
        
        df <- readxl::read_excel(file.path(inpath, info_and_carb_data_file_name), na = "")

    group_vars <- switch(
        mode,
        "by_site_and_period" = c("site_name", "period"),
        "by_site" = "site_name",
        "by_period" = "period"
    )
    
    # case_when_string <- paste( "case_when(", buildCondition(dry_wet_conditions), ")" ) # with feet
    case_when_string <- paste( "case_when(", buildCondition(dry_wet_conditions[-8,]), ")" ) # without feet

    dry_or_wet <- df %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
        dplyr::mutate(
            state = eval(parse_expr(case_when_string))
        )

    summary <- dry_or_wet %>%
        dplyr::summarize(
            .groups = "drop",
            DRY = round(sum(state == -3) + 0.75*sum(state == -2) + 0.5*sum(state == -1), 0),
            WET = round(sum(state == 3) + 0.75*2*sum(state == 2) + 0.5*sum(state == 1), 0),
            na = sum(state == 0 ),
            total = sum(!is.na(state)),
            ident_rate = paste(round(100*(sum(!is.na(state)) - sum( state == 0 ))/sum(!is.na(state)), 0), "%"),
            dry3 = sum(state == -3),
            dry2 = sum(state == -2),
            dry1 = sum(state == -1),
            wet3 = sum(state == 3),
            wet2 = sum(state == 2),
            wet1 = sum(state == 1)
        )

    plot_data <- summary %>%
        tidyr::pivot_longer(
            cols = c(DRY, WET),
            names_to = "condition",
            values_to = "value"
        ) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars)), condition) %>%
        dplyr::summarise(total = sum(value), .groups = 'drop') %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
        dplyr::mutate(percentage = total / sum(total) * 100)

    if(mode != "by_site") {
        plot_data <- plot_data %>%
            dplyr::mutate(period = factor(period, levels = c("Shang", "Zhou")))
    }

    facet_formula <- paste( "~", paste(group_vars, collapse = " + " ))

    piecharts <- 
        ggplot2::ggplot(
            plot_data,
            ggplot2::aes(y = percentage, fill = condition)
        ) +
        ggplot2::geom_bar(
            ggplot2::aes(x = factor(1)),
            stat = "identity",
            width = 1,
            colour = "white",
            linewidth = 1
        ) +
        ggplot2::geom_text(
            ggplot2::aes(x = factor(1), label = paste0(round(percentage), "%")),
            position = position_stack(vjust = 0.5),
            size = 4,
            color = "white"
        ) +
        ggplot2::coord_polar("y", start = 0) +
        ggplot2::facet_wrap(as.formula(facet_formula), ncol = 2) +
        ggplot2::labs(
            x = NULL,
            y = NULL,
            fill = "Condition",
            title = sprintf("DRY versus WET by %s", paste(group_vars, collapse = " and "))
        ) +
        ggplot2::scale_fill_manual(values = drywetColors) +
        hrbrthemes::theme_ipsum(base_family = "opensans") +
        ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank(),
            axis.title.x = ggplot2::element_blank(),
            axis.title.y = ggplot2::element_blank(),
            strip.text = ggplot2::element_text(size = 15, face = "bold", hjust = 0.5),
            legend.position = "bottom",
            panel.spacing = ggplot2::unit(2, "lines"),
            plot.title = ggtext::element_markdown(size = 20, face = "bold", hjust = 0.5)
        )

    stat_data <- summary %>%
        dplyr::mutate(
            group = do.call(paste, c(dplyr::select(., dplyr::all_of(group_vars)), sep = ", "))
        ) %>%
        dplyr::select(group, DRY, WET)

    data_matrix <- as.matrix(stat_data[, c("DRY", "WET")])
    rownames(data_matrix) <- stat_data$group
    ftest <- fisher.test(data_matrix, alternative = "two.sided")
    pvalue <- format(ftest$p.value, scientific = FALSE, digits = 3)

    conditions <- dry_wet_conditions %>%
        dplyr::select(header, help, value)


    dryCOLindex <- which(names(summary) == "DRY")
    wetCOLindex <- which(names(summary) == "WET")
    
    
    table_sum <- 
        ggpubr::ggtexttable(
            summary,
            rows = NULL,
            theme = ggpubr::ttheme(
                "light",
                padding = ggplot2::unit(c(1.5, 6), "mm")
                )
        ) %>%
        ggpubr::table_cell_bg(
            row = 2:(nrow(summary) + 1),
            column = wetCOLindex,
            fill = colorspace::lighten(unname(drywetColors["WET"]), amount = 0.3),
            color = "white"
        ) %>%
        ggpubr::table_cell_bg(
            row = 2:(nrow(summary) + 1),
            column = dryCOLindex,
            fill = colorspace::lighten(unname(drywetColors["DRY"]), amount = 0.2),
            color = "white"
        )

    table_cond <- 
        ggpubr::ggtexttable(
            conditions,
            rows = NULL,
            theme = ggpubr::ttheme(
                "light",
                padding = ggplot2::unit(c(4, 6), "mm")
            )
        ) %>%
        ggpubr::table_cell_bg(
            row = 2:4,
            column = 1,
            fill=lighten(unname(drywetColors["DRY"]), amount = 0.2),
            color = "white"
        ) %>%
        ggpubr::table_cell_bg(
            row = 5:9,
            column = 1,
            fill=lighten(unname(drywetColors["WET"]), amount = 0.3),
            color = "white"
        )

    left_col <- (table_cond + theme(plot.margin = unit(c(0,0,100,0), "pt")))  / table_sum 
    combined_plot <- left_col | piecharts

    results <- list (data = dry_or_wet, plot = combined_plot)
    return(results)
}