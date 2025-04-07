combinations <- raw_df %>%
    distinct(site = site_name, period)

periods <- data.frame(
    period = as.vector(unique(combinations$period)),
    site = rep("all sites", length(unique(combinations$period)))
)

sites <- data.frame(
    period = rep("all periods", length(unique(combinations$site))),
    site = as.vector(unique(combinations$site))
)

combinations <- bind_rows(combinations, periods, sites)



var_summary <- function(df, var, site, period) {

    if (!!site == "all sites" | !!period == "all periods" ) {
        if (!!site == "all sites" & !!period == "all periods") {
            data <- df %>% filter( !is.na(.data[[var]]))
        } else if (!!site == "all sites") {
            data <- df %>% filter(period == !!period & !is.na(.data[[var]])) 
        } else {
            data <- df %>% filter(site_name == !!site & !is.na(.data[[var]])) 
        }
    } else {
        data <- df %>% filter(site_name == !!site & period == !!period & !is.na(.data[[var]]))
    }
    print(nrow(data))
    if (nrow(data) == 0) {
            return(NULL)
        }


    data <- data %>% select(.data[[var]])
    
    stat <- data.frame(
        Count = nrow(data),
        Min = round(min(as.vector(unlist(data))), 1),
        Median = round(median(as.vector(unlist(data))), 1),
        Mean = round(mean(as.vector(unlist(data))), 1),
        SD = round(sd(as.vector(unlist(data))), 1),
        Max = round(max(as.vector(unlist(data))), 1)
    )
    stat_t <- data.frame(
        Stat = c("Count", "Min", "Median", "Mean", "SD", "Max"),
        Value = as.vector(t(stat))
    )
    annotation_text <- paste(stat_t$Stat, ": ", stat_t$Value, collapse = "\n")

    # histogram
    hist_plot <- ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
        labs(x = "Value", y = "Frequency") +
        theme_minimal() + 
        theme(
            panel.grid = element_blank(),
            axis.line = element_line(color = "black"),
            plot.margin = unit(c(1, 1, 1, 1), "lines")# Adjust margins as needed
        ) +
        annotate("text", x = Inf, y = Inf, label = annotation_text, hjust = 1, vjust = 1, size = 3)

    return(hist_plot)
}

# Test the function
temp <- as.data.frame(raw_df)
# plot_object <- var_summary(temp, "cord_mark_width", "all sites", "all periods")

#var_summary(temp, "cord_mark_width", "Kanjiazhai", "Shang")



row1 <- textGrob("orifice", gp = gpar(fontsize = 20, fontface = "bold", col = "blue"))
row2 <- textGrob("Kanjiazhai ", gp = gpar(fontsize = 20), rot = 90)
row3 <- textGrob("Gaoqingcaopo", gp = gpar(fontsize = 20), rot = 90)
row4 <- textGrob("Yangxinliwu", gp = gpar(fontsize = 20), rot = 90)
row5 <- textGrob("all sites", gp = gpar(fontsize = 20), rot = 90)
col6 <- textGrob("Shang ", gp = gpar(fontsize = 20))
col11 <- textGrob("Zhou", gp = gpar(fontsize = 20))
col16 <- textGrob("all periods", gp = gpar(fontsize = 20))

plot7 <- var_summary(temp, "orifice", "Kanjiazhai", "Shang")
plot8 <- var_summary(temp, "orifice", "Gaoqingcaopo", "Shang")
plot9 <- var_summary(temp, "orifice", "Yangxinliwu", "Shang")
plot10 <- var_summary(temp, "orifice", "all sites", "Shang")

plot11 <- var_summary(temp, "orifice", "Kanjiazhai", "Zhou")
plot12 <- var_summary(temp, "orifice", "Gaoqingcaopo", "Zhou")
plot13 <- var_summary(temp, "orifice", "Yangxinliwu", "Zhou")
plot14 <- var_summary(temp, "orifice", "all sites", "Zhou")

plot17 <- var_summary(temp, "orifice", "Kanjiazhai", "all periods")
plot18 <- var_summary(temp, "orifice", "Gaoqingcaopo", "all periods")
plot19 <- var_summary(temp, "orifice", "Yangxinliwu", "all periods")
plot20 <- var_summary(temp, "orifice", "all sites", "all periods")


plots <- list(plot7, plot8, plot9, plot10, plot11, plot12, plot13, plot14, plot17, plot18, plot19, plot20)
plots <- lapply(plots, function(p) if (is.null(p)) nullGrob() else p)

# Define the layout matrix
layout_matrix <- rbind(
    c(1, 6, 11, 16),
    c(2, 7, 12, 17),
    c(3, 8, 13, 18),
    c(4, 9, 14, 19),
    c(5, 10, 15, 20)
)

widths <- c(0.5, 2, 2, 2) 
heights <- c(0.5, 2, 2, 2, 2)

# Arrange the grid with specified widths
grid.arrange(
   #top = textGrob("Daily QC: Blue",gp=gpar(fontsize=20,font=3)),
grobs = list(
    row1, row2, row3, row4, row5,
    col6, plots[[1]], plots[[2]], plots[[3]], plots[[4]],
    col11, plots[[5]], plots[[6]], plots[[7]], plots[[8]],
    col16, plots[[9]], plots[[10]], plots[[11]], plots[[12]]
    ),
layout_matrix = layout_matrix,
widths = widths,
heights=heights
)

           
