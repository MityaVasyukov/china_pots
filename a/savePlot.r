save_plot <- function(plot_function, table, modeG = "by_site_and_period") {
    plot_data <- plot_function(table, mode = modeG)
    grid.draw(plot_data$plot)

    file_name <- paste0(getwd(), "/output/", plot_data$name, ".", file_format)
    ggsave(file_name, plot_data$plot, device = file_format, create.dir = TRUE, width = 20, height = 20)
 
}





# drywet:


#save_plot <- function(plot_function, table, modeG = "by_site_and_period") {
#    plot_data <- plot_function(table, mode = modeG)
#    print(plot_data$plot)
    

##    file_name <- paste0(getwd(), "/output/", plot_data$name, ".", file_format)
 #   ggsave(file_name, plot_data$plot, device = file_format, create.dir = TRUE, width = 10, height = 10)
    
 #   cat("Plot saved successfully!\n")
#}
