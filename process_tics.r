## ---- processtics ----
process_tics <- function(
                    data = NULL,
                    index_tbl = NULL,
                    only_high_tic = TRUE,
                    expander = 10 ) {
    
    if (is.null(data) || is.null(index_tbl)) stop("No data")

    # loadings
        # dependencies
            source("settings.r")
            savepath <- ifelse(exists("outpath"), outpath, NULL)
            source("load_pckgs.r")
            source("pc_plotter.r")
            dependencies <-  c("dplyr", "tidyr", "purrr", "IRanges", "e1071", "MASS", "ggplot2", "patchwork", "ggplotify", "ggpubr", "cowplot")
            invisible(lapply(dependencies, load_pkg))
            
            lookup_base_mz <- function(rt) {
                all_peaks %>%
                    dplyr::filter(floor(start_time) <= ceiling(rt), ceiling(end_time) >= floor(rt)) %>%
                    dplyr::pull(base_mz)
            }

            all_peaks <- dplyr::bind_rows(data$peaks)

    # interpolating
        message("\nInterpolating...")

        # Determine the common time range
            common_min <- max(sapply(data$tics, function(df) min(df$time)))
            common_max <- min(sapply(data$tics, function(df) max(df$time)))
            n_points <- min(sapply(data$tics, nrow))
            max_len_dif <- max(sapply(data$tics, nrow)) - n_points

        # Define a common time grid (choose the number of points based on your desired resolution)
            common_time <- seq(common_min, common_max, length.out = n_points)
            cat("\tCommon time length set to ", n_points, "; the range is from ", common_min, "to", common_max, "\n")
            cat("\tMaximum length difference between samples:", max_len_dif, "\n")

        # Resample every TIC using linear interpolation:
            tic_interp <- purrr::map(data$tics, function(df) {
                approx(x = df$time, y = df$tic, xout = common_time)$y
            })
            
            tic_matrix <- do.call(rbind, tic_interp)
            colnames(tic_matrix) <- as.character(round(common_time, 2))
            cat("\tDimensions of interpoled tic matrix:", dim(tic_matrix), "\n")

    # subset to only high tics
        if (only_high_tic) {
            message("\nSubsetting to high tics...")
            hi_tic_ranges <- all_peaks %>%
                dplyr::select(start_index, end_index) %>%
                { IRanges::IRanges(start = .$start_index, end = .$end_index) } %>%
                IRanges::reduce() %>%
                as.data.frame() %>%
                dplyr::as_tibble() %>%
                dplyr::arrange(start) %>%
                dplyr::mutate( # expand the range just for case
                    start = start - expander,
                    end = end + expander,
                    width = end - start
                )

            selected_scans <- unlist(lapply(seq_len(nrow(hi_tic_ranges)), function(i) {
                hi_tic_ranges$start[i]:hi_tic_ranges$end[i]
            }))

            cat("\tNumber of time ranges:", nrow(hi_tic_ranges), "; total range:", sum(hi_tic_ranges$width), "\n")
            tic_matrix <- tic_matrix[, selected_scans, drop = FALSE]
            cat("\tDimensions of subsampled tic matrix:", dim(tic_matrix), "\n")
        }

    # save the originbal colnames (ret time indices)
        colnames <- colnames(tic_matrix)

    # Box-Cox transformation and scaling
        message("\nBox-Coxing and scaling...")
        tic_matrix_boxcox <- apply(tic_matrix, 2, function(x) {
            bc <- boxcox(x ~ 1, plotit = FALSE)
            lambda <- bc$x[which.max(bc$y)]
            
            if (abs(lambda) < 1e-6) {
                # When lambda is very close to 0, use logarithm
                return(log(x))
            } else {
                return((x^lambda - 1) / lambda)
            }
        })

        tic_matrix <- matrix(tic_matrix_boxcox, nrow = nrow(tic_matrix), ncol = ncol(tic_matrix))
        
        tic_matrix <- scale(tic_matrix)
        
        colnames(tic_matrix) <- colnames # restore the colnames

    # Check the skewness
        skew_values <- apply(tic_matrix, 2, e1071::skewness)
        cat("\tSkewness:\n")
        print(summary(skew_values))

        if (skew_values[4] > 1) stop("Skewness is larger than 1...")

    # PCA
        # analysis
            message("\nPrincipal Component Analysis...")
            pca_result <- prcomp(tic_matrix, center = FALSE, scale. = FALSE)
            pca_summary <- summary(pca_result)
            cat("\tSummary of the first five principal components:\n")
            print(pca_summary$importance[, 1:5])
            #plot( # The variance explained by PCs
            #    pca_result,
            #    type = "l",
            #    main = "Variance, explained by PCAs"
            #    )

        # visualization
            # Prepare the PCA data for visualisation
                message("\nCooking the plot...")
                pca_df <- as.data.frame(pca_result$x)
                pca_df$file <- names(data$tics)
                pca_plot <- index_tbl %>% 
                    dplyr::mutate(file = basename(path)) %>% 
                    dplyr::select(file, period, site) %>%
                    dplyr::inner_join(pca_df, by = "file")

            # PC1 vs PC2 scores
                p1_title <- ifelse(
                    only_high_tic,
                    "TIC principal component analysis, subseted for high tics",
                    "TIC principal component analysis"
                    )
                
                p1 <- ggplot2::ggplot(
                        pca_plot,
                        ggplot2::aes(x = PC1, y = PC2, color = period, shape = site)
                    ) +
                    ggplot2::geom_point(size = 3) +
                    ggplot2::theme_minimal() +
                    ggplot2::labs(
                        title = p1_title,
                        x = "PC1",
                        y = "PC2"
                        )

            # first PCs loadings
                t <- pc_plotter(pca_result, pc = 1)
                p2 <- t$plot
                top_loadings <- t$sel_loadings
                
                t <- pc_plotter(pca_result, pc = 2)
                p3 <- t$plot
                top_loadings <- c(top_loadings, t$sel_loadings)

            # create a table of ret times and baze m/z-s which are responsible for the observed clustering
                noteworthy <- dplyr::tibble(
                        ret_time = sort(top_loadings),
                        base_mz = purrr::map_chr(
                            sort(top_loadings), ~ {
                                matching <- lookup_base_mz(.x)
                                if(length(matching) == 0) { NA } else { paste(unique(matching), collapse = ", ") }
                            })
                        )

            # split the table
                mid <- ceiling(nrow(noteworthy) / 2)
                ntwrth_left  <- noteworthy[1:mid, ]
                ntwrth_right <- noteworthy[(mid + 1):nrow(noteworthy), ]

            # Create table grobs
                p4_left <- ggpubr::ggtexttable(ntwrth_left, rows = NULL, theme = ggpubr::ttheme("minimal")) +
                    ggplot2::theme(plot.margin = ggplot2::margin(5, 5, 5, 5))
                p4_right <- ggpubr::ggtexttable(ntwrth_right, rows = NULL, theme = ggpubr::ttheme("minimal")) +
                    ggplot2::theme(plot.margin = ggplot2::margin(5, 5, 5, 5))

                p4_table <- cowplot::plot_grid(p4_left, p4_right, ncol = 2, align = "hv")

                p4_title <- cowplot::ggdraw() +
                    cowplot::draw_label("Noteworthy peaks", fontface = 'bold', size = 16, x = 0.5, hjust = 0.5)

                p4 <- cowplot::plot_grid(p4_title, p4_table, ncol = 1, rel_heights = c(0.12, 1))

            # Combine the plots
                if (only_high_tic) {
                    combined_plot <- p3 + p1 + p4 + p2 + plot_layout(ncol = 2, nrow = 2)
                } else {
                    combined_plot <- p1
                }

        results <- list(plot = combined_plot)
    return(results)
}