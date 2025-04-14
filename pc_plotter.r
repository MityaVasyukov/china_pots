pc_plotter <- function(pca_data, pc = 1, threshold_p = 0.95, intergap = 10) {
    if (is.null(pca_data)) stop("No data")

    # loadings
        source("settings.r")
        source("load_pckgs.r")
        dependencies <-  c("dplyr", "tibble", "ggplot2")
        invisible(lapply(dependencies, load_pkg))
        data <- pca_data
        pc <- as.numeric(pc)
    
    # data
        pc_df <- tibble::tibble(
            time = as.numeric(rownames(data$rotation)),
            loading = data$rotation[, pc]
        )
    
    # calculate the threshold for loading input as 95% of max loading to the max loading
        threshold <- threshold_p * max(abs(pc_df$loading))
    
    # select distinct times that show max loadings
        selected_ret_times <- pc_df %>%
            dplyr::filter(abs(loading) >= threshold) %>%
            dplyr::arrange(time) %>%
            dplyr::mutate(gap = time - lag(time, default = time[1])) %>%
            dplyr::mutate(group = cumsum(gap > intergap)) %>%
            dplyr::group_by(group) %>%
            dplyr::slice(which.max(abs(loading))) %>%
            dplyr::ungroup() %>%
            dplyr::pull(time)

    # Plotting the spectrum plot:
        p <- ggplot(pc_df,aes(x = time, y = loading, color = abs(loading))) +
            geom_point() +
            scale_color_viridis_c(option = "plasma", direction = -1) +
            labs(
                title = paste0("PC", pc, " loadings"),
                subtitle = paste0("Dashed lines show ret times with ", threshold_p, " to max loadings"),
                x = "Retention Time (s)",
                y = paste0("PC", pc, " Loading"),
                color = "Absolute\nLoading"
            ) +
            theme_minimal() +
            geom_vline(xintercept = selected_ret_times, color = "red", linetype = "dashed") +
            theme(
                legend.position = "None"
            )

    results <- list(sel_loadings = selected_ret_times, plot = p)
    return(results)
}