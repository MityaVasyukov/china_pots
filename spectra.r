fetch_spectra <- function(path, inpath) {
    # parameters
        mz_bins <- 0:500
    # loadings
        # dependencies
            source("settings.r")
            source("load_pckgs.r")
            dependencies <-  c("readxl", "writexl", "dplyr", "stringr", "tidyr", "purrr", "stringr", "ggplot2", "mzR", "baseline", "rgl")
            invisible(lapply(dependencies, load_pkg))
            
        # alternative mzR loading
            #    if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
            #    BiocManager::install("mzR")
            #    library(mzR)

        # metadata
            df <- readxl::read_excel(file.path(inpath, info_and_carb_data_file_name), na = "") %>%
                dplyr::ungroup() %>%
                dplyr::rename(
                    site = site_name,
                    id = aux_id
                ) %>%
                dplyr::select(id, period, site)
        
        # data
            mzml_list <- list.files(path = path, pattern = "\\mzML$", full.names = TRUE)

            mzml_tbl <- tibble(path = mzml_list) %>%
                mutate(
                    filename = basename(mzml_list),
                    prefix = str_extract(filename, "^[A-Za-z]"),
                    num  = str_extract(filename, "(?<=^[A-Za-z])\\d+"),
                    postfix  = str_extract(filename, "(?<=\\d)(.*)(?=\\.mzML)"),
                    id = paste0(prefix, num)
                ) %>%
                select(path, id)

            data <- inner_join(df, mzml_tbl, by = "id")
            cat("📦 Number of mzMl files in the specified path: ", nrow(data))


# ----- DETERMINE MINIMAL NUMBER OF SCANS AMONG FILES -----
    min_num_scans <- Inf
    for (f in data$path) {
        msdata_temp <- openMSfile(f)
        headers_temp <- header(msdata_temp)
        num_scans_temp <- nrow(headers_temp)
        if (num_scans_temp < min_num_scans) {
            min_num_scans <- num_scans_temp
        }
        close(msdata_temp)
    }
    cat("\nMin number of scans among all files: ", min_num_scans, "\n")
    Sys.setenv(TZ = "Asia/Jerusalem")
    message("🕒 Estimated end time: ", format((Sys.time() + nrow(data)*165), format = "%H:%M:%S"))

    # Processing
        spectra_list <- list()

        # Loop over each file from the joined data frame
            for (file_to_read in data$path) {
                cat("\nStart processing: ", basename(file_to_read))
                
                # Open file and extract scan information
                    msdata <- openMSfile(file_to_read)
                    headers <- header(msdata)
                    retention_times <- headers$retentionTime
                    num_scans <- nrow(headers)
                    num_bins <- length(mz_bins)
            
                # Create an empty matrix: rows are scans (retention times) and columns are m/z bins
                    intensity_matrix <- matrix(0, nrow = num_scans, ncol = num_bins)
                    colnames(intensity_matrix) <- as.character(mz_bins)
                    rownames(intensity_matrix) <- retention_times

                # Loop over each scan and extract/accumulate peak intensities
                    cat("\nStart fetching data from ", basename(file_to_read))
                    for (i in 1:num_scans) {
                        # Extract peaks: column 1 = m/z; column 2 = intensity
                            peaks_mat <- mzR::peaks(msdata, i)
                        
                        # Filter peaks to only those with m/z values between 0 and 500
                            valid_idx <- which(peaks_mat[, 1] >= mz_bins[1] & peaks_mat[, 1] <= mz_bins[length(mz_bins)])
                            
                            if (length(valid_idx) > 0) {
                                valid_peaks <- peaks_mat[valid_idx, ]
                            
                                # In case there is only one peak, ensure that valid_peaks is a matrix
                                    if (is.null(dim(valid_peaks))) {
                                        valid_peaks <- matrix(valid_peaks, ncol = 2)
                                    }
                                
                                # Round the m/z values to the nearest integer to determine the appropriate bin
                                    bin_indices <- round(valid_peaks[, 1])
                        
                                # Ensure that the bin indices fall between 0 and 500 (inclusive)
                                    bin_indices[bin_indices < mz_bins[1]] <- mz_bins[1] #!
                                    bin_indices[bin_indices > mz_bins[length(mz_bins)]] <- mz_bins[length(mz_bins)] #!
                        
                                # Loop over each valid peak and accumulate the intensity in the corresponding m/z bin.
                                # If multiple peaks are rounded to the same integer, their intensities are summed.
                                    for (j in seq_along(bin_indices)) {
                                        bin <- bin_indices[j]
                                        # Since our matrix columns are named by the m/z value as characters, we convert 'bin' to a character
                                        intensity_matrix[i, as.character(bin)] <- intensity_matrix[i, as.character(bin)] + valid_peaks[j, 2]
                                    }
                            }
                    }
            # interpolating
                cat("\nStart interpolating data ", basename(file_to_read))

                # Interpolate the intensity matrix onto a new time grid with min_num_scans points
                    new_time <- seq(min(retention_times), max(retention_times), length.out = min_num_scans)
                    new_intensity_matrix <- matrix(0, nrow = length(new_time), ncol = ncol(intensity_matrix))
                    colnames(new_intensity_matrix) <- colnames(intensity_matrix)
                    rownames(new_intensity_matrix) <- new_time
        
                # For each m/z bin (each column), interpolate its intensity values over the new retention time grid
                    for (col in 1:ncol(intensity_matrix)) {
                        new_intensity_matrix[, col] <- approx(
                            x = retention_times,
                            y = intensity_matrix[, col],
                            xout = new_time,
                            method = "linear",
                            rule = 2)$y
                    }

                # --- Scaling & Log Transformation ---
                    cat("\nNormalizing", basename(file_to_read))
                    # Scale the entire matrix so that the minimum becomes 0 and the maximum becomes 1.
                    global_min <- min(new_intensity_matrix)
                    global_max <- max(new_intensity_matrix)
                    scaled_matrix <- (new_intensity_matrix - global_min) / (global_max - global_min)
                    
                    # To avoid log(0), add a small offset epsilon before taking the logarithm.
                    epsilon <- 1e-6
                    log_scaled_matrix <- log(scaled_matrix + epsilon)
                    
                # Close the mzML file to free resources.
                    close(msdata)
                    cat("\nFinished processing: ", basename(file_to_read), "\n")

                # Store the resulting matrix in the list, using the file basename as name (or any identifier)
                    spectra_list[[basename(file_to_read)]] <- log_scaled_matrix
            }
    # Return the list with processed matrices for each file
        return(list(data = data, spectra = spectra_list))
}