
pca_analysis <- function(df, var) {
    # dependencies
        source("load_pckgs.r")
        dependencies <-  c("factoextra", "dplyr", "tidyr", "ggplot2")
        invisible(lapply(dependencies, load_pkg))
    
    # data transformation
        data <- df %>%
            dplyr::select(dplyr::all_of(var), dplyr::where(is.numeric)) %>%
            dplyr::select(-m.body_width, -m.height, -g.conf, -g.srr, -g.aapac18e_h) %>%
            tidyr::drop_na()

        message("📊 The data for PCA:")
        dplyr::glimpse(data)

        numeric_data <- data %>% dplyr::select(-dplyr::all_of(var))

    # analysis
        pca_result <- prcomp(numeric_data, center = TRUE, scale. = TRUE)
    
    # plot
        factoextra::fviz_pca_ind(pca_result,
                    label = "none",
                    habillage = data[[var]], 
                    addEllipses = TRUE,
                    ellipse.level = 0.95) +
        ggtitle("PCA: Individuals Colored by Combined Group")
}