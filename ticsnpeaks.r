ticsnpeaks <- function(index_tbl, sdnum = 3) {
    # loadings
        # dependencies
            source("settings.r")
            source("load_pckgs.r")
            dependencies <-  c("readxl", "dplyr", "mzR", "pracma")
            invisible(lapply(dependencies, load_pkg))

    # create empty lists
        tic_list <- list()
        peak_list  <- list()
  
    # loop over each sample in the index table
        for (file in index_tbl$path) {
            cat("Processing file:", basename(file), "\n")
            
            ms <- mzR::openMSfile(file)
            tic <- mzR::tic(ms)
            tic_list[[basename(file)]] <- tic

            threshold <- mean(tic$tic) + sdnum * sd(tic$tic)

            # Identify all indices above threshold
            idx_above <- which(tic$tic > threshold)

            if (length(idx_above) == 0) {
                cat("No peaks detected above the threshold.\n")
            } else {
                runs <- rle(diff(idx_above))  # find contiguous peaks
                break_positions <- which(runs$values != 1) # > 1 mean new peak region
                run_ends <- cumsum(runs$lengths)
                boundaries <- c(0, run_ends[break_positions], length(idx_above))
                
                #  Loop over each contiguous peaks to find start, max, and end
                peak_temp_list <- list()
                
                for (i in seq_along(boundaries)[-1]) {
                    start_in_run <- boundaries[i-1] + 1
                    end_in_run   <- boundaries[i]
                    these_idx    <- idx_above[start_in_run:end_in_run]
                    
                    peak_start <- min(these_idx)
                    peak_end   <- max(these_idx)

                    local_max_index <- which.max(tic$tic[these_idx])
                    peak_max <- these_idx[local_max_index]
                    
                    # Extract the spectrum at the scan with max tic
                        sp <- mzR::peaks(ms, scan = peak_max)
                        if (!is.null(sp) && nrow(sp) > 0) {
                            # Find the m/z value with the highest intensity in the spectrum
                            base_mz <- sp[which.max(sp[, 2]), 1]
                        } else {
                            base_mz <- NA
                        }

                    # Store results
                    peak_temp_list[[i]] <- tibble(
                            start_index  = peak_start,
                            max_index = peak_max,
                            end_index = peak_end,
                            start_time = tic$time[peak_start],
                            max_time = tic$time[peak_max],
                            end_time = tic$time[peak_end],
                            max_peak_tic = tic$tic[peak_max],
                            base_mz = base_mz
                        ) %>%
                        mutate(duration = end_time - start_time)
                }

                # Combine
                peak_info <- do.call(rbind, peak_temp_list)
                rownames(peak_info) <- NULL
                peak_list[[basename(file)]] <- peak_info
            }
            close(ms)
        }
    return(list(tics = tic_list, peaks = peak_list))
}