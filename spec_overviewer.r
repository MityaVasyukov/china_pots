spec_overviewer <- function(index_tbl, num) { 
    # loadings
        source("settings.r")
        source("load_pckgs.r")
        dependencies <- c("dplyr", "mzR", "ggplot2", "gridExtra", "magick")
        invisible(lapply(dependencies, load_pkg))
    
    # check if the specified file exists
        file_path <- index_tbl$path[[num]]
        if (!file.exists(file_path)) {
            stop("The file ", file_path, " does not exist.")
        }
        file_name <- basename(file_path)
    
    # open mzML file and extract info
        message("Fetching data...")
        ms <- openMSfile(file_path)
        hdr <- header(ms)
        peaks_data <- peaks(ms, 1)
        nScans <- nrow(hdr)

    # define the future plot parameters
        rt_min <- min(hdr$retentionTime)
        rt_max <- max(hdr$retentionTime)
       # nScans <- length(hdr$retentionTime)

        rt_bins <- seq(rt_min, rt_max, length.out = nScans + 1)
        rt_midpoints <- (rt_bins[-1] + rt_bins[-length(rt_bins)])/2
        rt_limits <- c(rt_min, rt_max)

        mz_min <- min(peaks_data[, 1])
        mz_max <- max(peaks_data[, 1])
        nBinsMz <- round(mz_max- mz_min) + 1
        mz_bins <- seq(mz_min, mz_max, length.out = nBinsMz + 1)
        mz_midpoints <- (mz_bins[-1] + mz_bins[-length(mz_bins)])/2
    
    # create an empty matrix to styore the intensities
        heatmap_matrix <- matrix(0, nrow = nBinsMz, ncol = nScans)
    
    # populate each scan's data to the heatmap matrix

        for (i in seq_len(nScans)) {
            rt <- hdr$retentionTime[i]
            rt_bin <- findInterval(rt, rt_bins, rightmost.closed = TRUE)
            if (rt_bin < 1 || rt_bin > nScans) next
            
            peaks_data <- peaks(ms, i)
            if (nrow(peaks_data) == 0) next
            
            for (j in 1:nrow(peaks_data)) {
            mz_val <- peaks_data[j, 1]
            intensity <- peaks_data[j, 2]
            mz_bin <- findInterval(mz_val, mz_bins, rightmost.closed = TRUE)
            if (mz_bin < 1 || mz_bin > nBinsMz) next
            
            heatmap_matrix[mz_bin, rt_bin] <- heatmap_matrix[mz_bin, rt_bin] + intensity
            }
        }
    
    # log-transformation
        heatmap_matrix_log <- log10(heatmap_matrix + 1)
    
    # plotting
        message("Plots building...")
        # plot 1
            heatmap <- data.frame(
                rt = rep(rt_midpoints, each = nBinsMz),
                mz = rep(mz_midpoints, times = nScans),
                intensity = as.vector(heatmap_matrix_log)
                )

            p1 <- ggplot(
                    heatmap,
                    aes(x = rt, y = mz, fill = intensity)
                ) +
                geom_tile() +
                scale_fill_gradient(
                    low = "white",
                    high = "black",
                    name = "log10(Intensity)"
                ) +
                labs(
                    title = paste("Intensity Spectrum for ", file_name, "(log10)"), 
                    x = "Retention Time (sec)",
                    y = "m/z"
                    ) +
                    scale_x_continuous(limits = rt_limits, expand = c(0, 0)) +
                theme_minimal() +
                theme(legend.position = "none")
    
        # plot 2 
            tic <- hdr$totIonCurrent
            df_tic <- data.frame(rt = hdr$retentionTime, tic = tic)
            
            p2 <- ggplot(
                df_tic,
                aes(x = rt, y = tic)
                ) +
                geom_line(col = "blue") +
                labs(
                    title = paste("Total Ion Current Chromatogram for ", file_name), 
                    x = "Retention Time (sec)",
                    y = "Total Ion Current"
                ) +
                scale_x_continuous(limits = rt_limits, expand = c(0,0)) +
                theme_minimal()
            
        # plot 3
            mass_spectrum <- rep(0, nBinsMz)

            for (i in 1:nScans) {
                peaks_data <- peaks(ms, i)
                if (nrow(peaks_data) == 0) next
                
                for (j in 1:nrow(peaks_data)) {
                    mz_val <- peaks_data[j, 1]
                    intensity <- peaks_data[j, 2]
                    mz_bin <- findInterval(mz_val, mz_bins, rightmost.closed = TRUE)
                    
                    if (mz_bin >= 1 && mz_bin <= nBinsMz) {
                        mass_spectrum[mz_bin] <- mass_spectrum[mz_bin] + intensity
                    }
                }
            }

            mass_spectrum <- data.frame(mz = mz_midpoints, intensity = mass_spectrum)

            p3 <- ggplot(
                mass_spectrum,
                aes(x = mz, y = intensity)
                ) +
                geom_line(col = "red") +
                labs(
                    title = paste("Summed Mass Spectrum for ", file_name),
                    x = "Summed Intensity",
                    y = "m/z"
                    ) +
                theme_minimal() +
                coord_flip()
    
    # close the mzML file
        mzR::close(ms)
    
    # grid plotting
        layout_matrix <- matrix(c(3, 1, NA, 2), nrow = 2, byrow = TRUE)
        combined_plot <- gridExtra::arrangeGrob(p1, p2, p3, layout_matrix = layout_matrix)
        save_path = file.path(outpath, paste0(sub("\\..*$", "", file_name), "_spectrum_overview.png"))

        ggsave(
            filename = save_path,
            plot = combined_plot,
            width = 16,
            height = 16,
            dpi = 300
        )

        message("Image saved to: ", basename(save_path))
        img <- magick::image_read(save_path)
        plot(img)
}