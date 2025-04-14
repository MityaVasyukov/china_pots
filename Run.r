###   SETTINGS AND LOADINGS   ###
    # Path settings
        wd <- getwd()
    # Load dependencies
        source("load_pckgs.r")
        source("settings.r")
        source("carbPlotter.r")
        source("dryOrWet.r")
        source("overviewer.r")
        source("pca_analysis.r")
        source("lda_analysis.r")
        source("ran_for_analysis.r")
        source("cluster_analysis.r")
        source("means_clust_analysis.r")

        load_pkg("readxl")
        load_pkg("dplyr")

    # Load data
    raw_df <- read_excel(file.path(inpath, info_and_carb_data_file_name), na = "")

###   ANALYSIS   ###
    # Run carbonization pattern plotter
        carb_plotter(raw_df)

    # Run dry/wet analysis
        dryOrWet(raw_df, "by_site_and_period")

    # Overview plotter through all the data
        # prepare the data

        # Run overviwer
        source("overviewer.r")
        r <- overviewer(inpath, outpath, talky =F)

        summary(r)
        print(r$numeric_plots$m.rim)
        lapply(r$numeric_plots, print)
        invisible(lapply(list.files(outpath, pattern = "\\.tiff$", full.names = TRUE), browseURL))


        source("pca_analysis.r")
        pca_analysis(r$data, "i.collection")
        pca_analysis(r$data, "i.period")
        pca_analysis(r$data, "i.site")

        source("lda_analysis.r")
        lda_analysis(r$data, "i.collection")
        

        # Spectral analysis
            path <- "p:/2025-vessels/spectra/mz"
            source("spec_checker.r")
            source("spec_overviewer.r")
            source("ticsnpeaks.r")
            
            spec_index <- spec_checker(inpath, path) # checks the files and returns the list of verified paths & metadata
            spec_overviewer(spec_index, 50) # provides a '3d' visualization of GCMS data for a given data file
            e <- ticsnpeaks(spec_index, 3) # fetches the tics for all spectra and the list of outstanding peaks
                summary(e)
                unlist(lapply(e$tics, nrow))
                unlist(lapply(e$peaks, nrow))

            source("process_tics.r")
            process_tics(data = e, index_tbl = spec_index, only_high_tic = T)









            source("spectra.r")

# Example usage:
# Provide the path to your GC–MS file. 
# If xcmsRaw supports reading mzML on your system, it should just work.
# Otherwise, you may need to convert your file to mzXML or netCDF.

heatmap_xcmsRaw(spec_index$path[1], binSize = 0.1)

           # path <- "p:/2025-vessels/temp"
            path <- "p:/2025-vessels/spectra/mz"
            source("peak_list.r")
            e <- peak_list(path, inpath)
            # PEASK AFTERPARTY
                library(tidyverse)

                # ONE - nrow peaks
                    # Create a data frame from e$data that also contains the corresponding file's basename.
                    # (We assume e$data is your tibble with columns id, period, site, and path.)
                    df <- e$data %>%
                    mutate(file = basename(path)) %>%  # extracts "Y20A.mzML", "Y117A.mzML", etc.
                    rowwise() %>%
                    # For each file, compute the number of peaks (i.e. number of rows in its tibble)
                    mutate(nPeaks = nrow(e$peaks[[file]])) %>%
                    ungroup() %>%
                    mutate(
                        group = paste0(period, ".", substr(site, 1, 1)))

                    # Summarize the number of peaks by site:
                    df_summary <- df %>%
                    group_by(group) %>%
                    summarise(
                        mean_peaks = mean(nPeaks),
                        sd_peaks   = sd(nPeaks)
                    )

                    # Create the ggplot: a bar plot of the mean peak count with error bars for ± sd.
                    ggplot(df_summary, aes(x = group, y = mean_peaks)) +
                    geom_bar(stat = "identity", fill = "skyblue") +
                    geom_errorbar(aes(ymin = mean_peaks - sd_peaks, ymax = mean_peaks + sd_peaks),
                                    width = 0.2) +
                    labs(
                        x = "Site",
                        y = "Mean Number of Peaks (± SD)",
                        title = "Mean ± SD of Peak Counts by Site"
                    ) +
                    theme_minimal()
                
                
                # TWO
                    # Build a data frame that links the metadata to summary statistics from each file.
                    df <- e$data %>%
                    # Extract the filename from the full path (e.g. "Y20A.mzML")
                    mutate(file = basename(path)) %>%
                    rowwise() %>%
                    # For each file, compute the peak count (number of rows) and average duration from its tibble
                    mutate(
                        nPeaks = nrow(e$peaks[[file]]),
                        avg_duration = mean(e$peaks[[file]]$duration)
                    ) %>%
                    ungroup() %>%
                    mutate(
                        group = paste0(period, ".", substr(site, 1, 1)))

                    # Group by "site" and summarize both variables by computing their means and standard deviations.
                    df_summary <- df %>%
                    group_by(group) %>%
                    summarise(
                        mean_peaks = mean(nPeaks),
                        sd_peaks = sd(nPeaks),
                        mean_duration = mean(avg_duration),
                        sd_duration = sd(avg_duration)
                    )

                    # Plot a scatterplot with error bars in both dimensions.
                    # - X axis: mean peak count (with horizontal error bars showing ±sd)
                    # - Y axis: mean duration (with vertical error bars showing ±sd)
                    ggplot(df_summary, aes(x = mean_peaks, y = mean_duration, label = group)) +
                    geom_point(size = 3, color = "blue") +
                    geom_errorbar(aes(ymin = mean_duration - sd_duration, ymax = mean_duration + sd_duration),
                                    width = 0.2) +
                    geom_errorbarh(aes(xmin = mean_peaks - sd_peaks, xmax = mean_peaks + sd_peaks),
                                    height = 0.2) +
                    geom_text(nudge_y = 0.1) +
                    labs(
                        x = "Mean Number of Peaks (± SD)",
                        y = "Mean Duration (± SD)",
                        title = "Mean Peak Count vs. Mean Duration by Site"
                    ) +
                    theme_minimal()

                # THREE
                    # Create a data frame from e$data with the extracted filename.
                    df_data <- e$data %>%
                    mutate(file = basename(path)) %>% 
                    select(file, period, site)

                    # Build the max_index table for each file in e$peaks.
                    df_max_index <- map_dfr(names(e$peaks), function(fname) {
                    tibble(
                        filename = fname,
                        max_index = e$peaks[[fname]]$max_index
                    )
                    })

                    # Join the max_index table with the metadata.
                    df_joined <- df_max_index %>%
                    left_join(df_data, by = c("filename" = "file")) %>%
                    mutate(group = paste0(period, ".", substr(site, 1, 1))) %>%
                    select(-period, -site) %>%
                    dplyr::arrange(max_index)
                    
                    library(tidyverse)

                    # Get candidate max_index values and unique groups from df_joined.
                    candidate_values <- sort(unique(df_joined$max_index))
                    group_values <- unique(df_joined$group)

                    # Create a grid with every candidate value and group combination.
                    candidate_grid <- expand_grid(max_index = candidate_values,
                                                group = group_values)

                    # For each candidate and group combination, count the number of samples 
                    # (rows in df_joined) where the group's max_index is within ±2 of the candidate.
                    df_count <- candidate_grid %>%
                    mutate(count = map2_int(max_index, group, ~ {
                        group_indices <- df_joined %>%
                        filter(group == .y) %>%
                        pull(max_index)
                        sum(abs(group_indices - .x) <= 2)
                    }))

                    # Pivot the table so that each row corresponds to a candidate max_index and 
                    # each column (group) shows the count of occurrences.
                    df_wide <- df_count %>%
                    pivot_wider(names_from = group, values_from = count)

                    # Display the resulting table.
                    print(df_wide)

                    library(tidyverse)

                    # 1. Filter out candidate rows where the sum across groups is 1 or less.
                    df_wide_filtered <- df_wide %>%
                    mutate(total = Shang.Y + Zhou.K + Zhou.G + Shang.G) %>%
                    filter(total > 1)

                    # 2. Pivot the filtered wide format data to a long format.
                    df_long_filtered <- df_wide_filtered %>%
                    pivot_longer(cols = c(Shang.Y, Zhou.K, Zhou.G, Shang.G),
                                names_to = "group", values_to = "count") %>%
                                dplyr::mutate(
                                    group = factor(
                                        group,
                                        levels = c("Shang.Y", "Shang.G", "Zhou.G", "Zhou.K"))
                                ) %>%
                    # We only need records where count > 0.
                    filter(count > 0)

                    # 3. Create the bubble plot:
                    ggplot(df_long_filtered, aes(x = max_index, y = group, size = count, color = count)) +
                    geom_point(alpha = 0.7) +
                    scale_size_continuous(range = c(3, 10)) +
                    labs(
                        title = "Retention times of the scans with maximal total ion counts (> mean+-3sd)",
                        subtitle = "Count >1 is selected. Ret times were allowed to retreat +-2 ",
                        x = "Retention time index",
                        y = NULL,
                        size = "Count",
                        color = "Count"
                    ) +
                    theme_minimal()























# Assuming your fetch_spectra() function has already run:
d <- fetch_spectra(path, inpath)


data = data, spectra = spectra_list

# --- Assume you have already run:
# spectra_list <- fetch_spectra(path, inpath)
# and that spectra_list is a named list of matrices 
# with names equal to the basenames of your mzML files.

# Step 1: Define group identifiers from the file basenames
sample_names <- names(d$spectra)
# Extract the first character (converted to uppercase) from each sample name
group_vector <- toupper(substr(sample_names, 1, 1))
unique_groups <- unique(group_vector)
cat("Detected groups: ", paste(unique_groups, collapse = ", "), "\n")

if (length(unique_groups) != 2) {
  stop("Expected exactly 2 groups based on the first letter of filenames, but found: ",
       paste(unique_groups, collapse = ", "))
}

# Optional: reorder the spectra_list (and group_vector accordingly)
ord <- order(sample_names)
spectra_list <- d$spectra[ord]
group_vector <- group_vector[ord]

# Step 2: Stack the spectra matrices into a 3D array
array_data <- simplify2array(spectra_list)
dims <- dim(array_data)  # dims: (retention time points) x (m/z bins) x (samples)
R <- dims[1]  # number of retention time points
C <- dims[2]  # number of m/z bins
n_samples <- dims[3]
cat("Stacked array dimensions: ", R, " x ", C, " x ", n_samples, "\n")

# Step 3: Calculate cell-wise p-values using a two-sample t-test with error handling
p_mat <- apply(array_data, c(1, 2), function(v) {
  # v is the vector of intensities for a given cell across all samples
  vals_group1 <- v[group_vector == unique_groups[1]]
  vals_group2 <- v[group_vector == unique_groups[2]]
  # Ensure that each group has at least two values to run t.test
  if (length(vals_group1) >= 2 && length(vals_group2) >= 2) {
    # Use tryCatch to handle errors from t.test (for example when data are constant)
    p_value <- tryCatch(
      t.test(vals_group1, vals_group2)$p.value,
      error = function(e) NA
    )
    return(p_value)
  } else {
    return(NA)
  }
})

# Step 4: Prepare data for visualization
# We'll convert the p-value matrix to a long-format data frame.
library(reshape2)
p_df <- melt(p_mat)
colnames(p_df) <- c("RetentionTime", "mZ", "p_value")

# Convert row and column names into numeric values
p_df$RetentionTime <- as.numeric(as.character(p_df$RetentionTime))
p_df$mZ <- as.numeric(as.character(p_df$mZ))

# Calculate -log10(p) to accentuate low p-values
p_df$negLogP <- -log10(p_df$p_value)
p_df$significant <- p_df$p_value < 0.05
# Replace p==0 values and compute -log10

p_df$p_value[p_df$p_value == 0] <- 1e-10  
p_df$negLogP <- -log10(p_df$p_value)

# Remove non-finite rows
p_df <- p_df[is.finite(p_df$negLogP) & is.finite(p_df$mZ) & is.finite(p_df$RetentionTime), ]

# Create the heatmap plot
heatmap_plot <- ggplot(p_df, aes(x = mZ, y = RetentionTime, fill = negLogP)) +
  geom_tile() +
  scale_fill_viridis_c(option = "viridis", 
                       name = expression(-log[10](p)),
                       na.value = "white") +
  theme_minimal() +
  labs(title = "Differential Analysis (Cell-wise) Between Groups",
       subtitle = "Groups derived from first letter of file basenames",
       x = "m/z bin", 
       y = "Retention Time") +
  geom_contour(aes(z = as.numeric(significant)), 
               color = "red", 
               breaks = c(0.5), 
               na.rm = TRUE) +
  coord_fixed()

# Display the plot
print(heatmap_plot)




unlist(lapply(e$tics, nrow)
+ )








