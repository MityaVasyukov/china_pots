## ---- overviewer ----
overviewer <- function(inpath, savepath, save = FALSE, talky = TRUE) {
    ### LOADINGS
        ## dependencies
            source("settings.r")
            source("load_pckgs.r")
            source("parse_bounds.r")
            source("sum_non_numeric.r")
            source("string_processor.r")
            source("dryOrWet.r")
            dependencies <-  c("readxl", "writexl", "dplyr", "tidyr", "stringr", "scales", "ggplot2", "Hmisc", "ggcorrplot", "grid", "gridExtra", "viridis")
            invisible(lapply(dependencies, load_pkg))

        ## data
            raw_df <- readxl::read_excel(file.path(inpath, info_and_carb_data_file_name), na = "")
            raw_petro <- readxl::read_excel(file.path(inpath, petro_data_file_name), na = "")
            raw_arch <- dryOrWet("by_site_and_period")$data %>% dplyr::ungroup()
            raw_gcms <- readxl::read_excel(file.path(inpath, gcms_data_file_name), sheet = "Data", col_names = T, na = "")
        
            #if (talky) {
            #    if (all(sapply(mget(c("raw_arch", "raw_gcms", "raw_petro")), function(x) nrow(x) > 0))) {
            #        message("✅ All tables were loaded and contain records.")
            #    } else {
            #        warning("❌ One or more tables are empty!")
            #    }
            #}

        ## result list preparing
            results <- list(
                data = NULL,
                numeric_data = NULL,
                non_numeric_data = NULL,
                numeric_plots = NULL,
                non_numeric_plots = NULL,
                merged_plots = NULL
            )

    ### PROCESSING DATA
        # raw_arch
            arch <- raw_arch %>%
                dplyr::ungroup() %>%
                dplyr::rename(
                    site = site_name,
                    id = aux_id,
                    conf = state
                ) %>%
                dplyr::mutate(
                    group = dplyr::case_when(
                        conf < 0 ~ "dry",
                        conf > 0 ~ "wet",
                        conf == 0 ~ NA
                        ),
                    group = factor(group, levels = c("dry", "wet"), ordered = TRUE),
                    conf = dplyr::case_when(
                        abs(conf) == 3 ~ 1,
                        abs(conf) == 2 ~ 0.5,
                        abs(conf) == 1 ~ 0.3
                        ),
                    site = factor(site, levels = c("Gaoqingcaopo", "Kanjiazhai", "Yangxinliwu"), ordered = FALSE),
                    period = factor(period, levels = c("Shang", "Zhou"), ordered = TRUE),
                    lip = !is.na(col_in_lip) | !is.na(col_out_lip),
                    neck = !is.na(col_in_neck) | !is.na(col_out_neck),
                    shoulder = !is.na(col_in_shoulder) | !is.na(col_out_shoulder),
                    body = !is.na(col_in_body) | !is.na(col_out_body),
                    foot = !is.na(col_in_foot) | !is.na(col_out_foot),
                    crotch = !is.na(col_out_crotch),
                    completeness = round(100 * (lip + neck + shoulder + body + foot + crotch) / 6),
                    dplyr::across(dplyr::starts_with("col_"), ~ factor(tolower(as.character(.)))),
                    collection = paste0(period, ".", substr(as.character(site), 1, 1))
                ) %>%
                dplyr::mutate(
                    collection = factor(collection, levels = c("Shang.Y", "Shang.G", "Zhou.G", "Zhou.K"))
                ) %>%
                dplyr::select(
                    -c(record_id, sample_number, part, mouth_present, foot_present, vis_alysis, notes, decoration, residue_sampled, petro_sampled, question_field)
                    ) %>%
                dplyr::rename_with(~ paste0("i.", .), c(collection, site, period, lip, neck, shoulder, body, foot, crotch, completeness, cord_mark_type)) %>%
                dplyr::rename_with(~ paste0("m.", .), c(body_width, height, rim,  orifice, cord_mark_width)) %>%
                dplyr::rename_with(~ paste0("c.", .), c(dplyr::starts_with("col_"), group, conf))

        # raw_petro
            petro <- raw_petro %>%
                dplyr::select(-Photo, -`Interesting Incs`, -Site, -Period, -`Dry/Wet`, -`Dry/Wet Simple`, -Group) %>%
                dplyr::rename(
                    id = `Sample #`,
                    optical_activity = `Optical Activity`,
                    rock_frags = `Rock Frags`,
                    frag_size = `Frag Size`,
                    frag_roundness = `Frag Roundness`,
                    mica = Mica,
                    group = `Group Simplified`
                ) %>%
                dplyr::mutate(
                    optical_activity = ifelse(stringr::str_starts(stringr::str_to_lower(optical_activity), "A"), TRUE, FALSE),
                    frag_size = stringr::str_replace_all(frag_size, "[^0-9.<>=+-]", ""),
                    frag_roundness = stringr::str_to_lower(frag_roundness)
                )

            frag_size_clean <- parse_bounds(petro$frag_size) %>%
                dplyr::rename(
                    lower_fr_bound = lower,
                    upper_fr_bound = upper,
                    frag_size = original
                )

            frag_shape_clean <- dplyr::tibble(
                original = c("and-subang", "ang-subang", "ang-subrnd", "and-subrnd", "subang-subrnd", "subang-rnd", "ang-rnd", "rnd"),
                lower_roundness_bound = c("angular", "angular", "angular", "angular", "subangular", "subangular", "angular", "round"),
                upper_roundness_bound = c("subangular", "subangular", "subround", "subround", "subround", "round", "round", "round")
                )

            rock_dictionary <- petro %>%
                dplyr::filter(!is.na(rock_frags)) %>%
                dplyr::mutate(rock_frags = stringr::str_replace_all(rock_frags, " and ", ";")) %>%
                tidyr::separate_rows(rock_frags, sep = ";|/|-") %>%
                dplyr::mutate(rock_frags = stringr::str_trim(rock_frags)) %>%
                dplyr::filter(rock_frags != "") %>%
                dplyr::distinct(rock_frags)

            petro <- dplyr::bind_cols(petro, frag_size_clean) %>%
                dplyr::left_join(frag_shape_clean, by = c("frag_roundness" = "original")) %>%
                dplyr::mutate(
                    lower_roundness_bound = factor(
                        lower_roundness_bound,
                        levels = c("angular", "subangular", "subround", "round"),
                        ordered = TRUE
                        ),
                    upper_roundness_bound = factor(
                        upper_roundness_bound,
                        levels = c("angular", "subangular", "subround", "round"),
                        ordered = TRUE
                        ),
                    mica = factor(
                        mica,
                        levels = c("None", "No", "Rare", "Few", "Moderate", "Yes"),
                        ordered = TRUE
                        ),
                    group = factor(group),
                    granite = ifelse(!is.na(rock_frags) & stringr::str_detect(rock_frags, regex("granit", ignore_case = TRUE)), TRUE, FALSE),
                    granodiorite = ifelse(!is.na(rock_frags) & stringr::str_detect(rock_frags, regex("granodiorite", ignore_case = TRUE)), TRUE, FALSE),
                    diorite = ifelse(!is.na(rock_frags) & stringr::str_detect(rock_frags, regex("diorite", ignore_case = TRUE)), TRUE, FALSE),
                    sandstone = ifelse(!is.na(rock_frags) & stringr::str_detect(rock_frags, regex("sandstone", ignore_case = TRUE)), TRUE, FALSE),
                    mudstone = ifelse(!is.na(rock_frags) & stringr::str_detect(rock_frags, regex("mudstone", ignore_case = TRUE)), TRUE, FALSE),
                    limestone = ifelse(!is.na(rock_frags) & stringr::str_detect(rock_frags, regex("limestone", ignore_case = TRUE)), TRUE, FALSE),
                    gritstone = ifelse(!is.na(rock_frags) & stringr::str_detect(rock_frags, regex("gritstone", ignore_case = TRUE)), TRUE, FALSE),
                    volcanic = ifelse(!is.na(rock_frags) & stringr::str_detect(rock_frags, regex("volcanic", ignore_case = TRUE)), TRUE, FALSE),
                    andesite = ifelse(!is.na(rock_frags) & stringr::str_detect(rock_frags, regex("andesite", ignore_case = TRUE)), TRUE, FALSE),
                    microgranite = ifelse(!is.na(rock_frags) & stringr::str_detect(rock_frags, regex("microgranite", ignore_case = TRUE)), TRUE, FALSE),
                    ksp = ifelse(!is.na(rock_frags) & stringr::str_detect(rock_frags, regex("ksp", ignore_case = TRUE)), TRUE, FALSE)
                ) %>%
                dplyr::select( -frag_roundness, -rock_frags, - dplyr::starts_with("frag_size")) %>%
                dplyr::rename_with(~ paste0("p.", .), -id)

        # raw_gcms
            chumpsy_vars <- c("aapa", "phy", "tmtd", "cholesterol_bi_products", "miliacin", "ergostanol", "dha", "lc_ketones", "pah", "bpca", "contaminants")
            gcms <- raw_gcms %>%
                dplyr::rename_with(tolower) %>%
                dplyr::rowwise() %>%
                dplyr::rename(
                    lipids_conc = ug_g,
                    group = preliminary_interpretation,
                    conf = level_of_confidence,
                    srr = `srr_%` 
                    ) %>%
                dplyr::mutate(
                    id = sub("A$", "", id),
                    plant_count = sum(tolower(dplyr::c_across(dplyr::all_of(chumpsy_vars))) == 'p', na.rm = TRUE),
                    tree_count = sum(tolower(dplyr::c_across(dplyr::all_of(chumpsy_vars))) == 'tr', na.rm = TRUE),
                    cereal = ifelse(!is.na(comment) & stringr::str_detect(comment, regex("cereal", ignore_case = TRUE)), TRUE, FALSE),
                    fruit = ifelse(!is.na(comment) & stringr::str_detect(comment, regex("fruit", ignore_case = TRUE)), TRUE, FALSE),
                    vegetable = ifelse(!is.na(comment) & stringr::str_detect(comment, regex("vegetab", ignore_case = TRUE)), TRUE, FALSE),
                    resin = ifelse(!is.na(comment) & stringr::str_detect(comment, regex("resin", ignore_case = TRUE)), TRUE, FALSE),
                    fish = ifelse(!is.na(comment) & stringr::str_detect(comment, regex("fish", ignore_case = TRUE)), TRUE, FALSE),
                    complex_mxtr = ifelse(!is.na(comment) & stringr::str_detect(comment, regex("complex", ignore_case = TRUE)), TRUE, FALSE),
                    conf = dplyr::case_when(
                        conf == 1 ~ 1,
                        conf == 2 ~ 0.5,
                        conf == 3 ~ 0.3
                        ),
                    group = dplyr::case_when(
                        group == 1 ~ "plant",
                        group == 2 ~ "mixture",
                        group == 3 ~ "animal",
                        ),
                    group = factor(
                        group,
                        levels = c("plant", "mixture", "animal"),
                        ordered = TRUE
                        )
                    ) %>%
                dplyr::ungroup() %>%
                dplyr::select(-dynastie, -dplyr::all_of(chumpsy_vars), -comment) %>%
                dplyr::rename_with(~ paste0("g.", .), -id)

        # All the tables are ready for joining nopw
           info.variable_prefix <- c(
                "ℹ️ Prefixes were added to each variable name addressing their function or derivation",
                "\t i.* = Information about the specimen",
                "\t m.* = Measurements of the specimen",
                "\t c.* = Carbonization coloration analysis",
                "\t g.* = GCMS data",
                "\t p.* = Petrography analysis data"
            )

            #if (talky) {
            #    for (line in info.variable_prefix) {
            #        cat(line, "\n")
            #    }
            #}

        # Fusing
            data <- arch %>%
                dplyr::inner_join(gcms, by = "id") %>%
                dplyr::inner_join(petro, by = "id")
            
            if (nrow(data) == 0) {
                stop("❌ data has no records!")
            } else {
                results$data <- data
            }

            

            if (talky) {
                message("⚠️ inner joining was used to link the data, use left_join to conserve unmatched records")
                
                intersected_ids <- length(Reduce(intersect, list(arch$id, gcms$id, petro$id)))
            
                cat("Petrography:", nrow(petro), "records\n")
                cat("GCMS:", nrow(gcms), "records\n")
                cat("Carbonisation:", nrow(arch), "records\n\n")
                cat("Max number of intersected id-s:", intersected_ids, "\n")
                cat("Number of records in the final table:", nrow(data), "\n")
                #dplyr::glimpse(data)
                cat("\n\n📦 Variable Descriptions:\n\n")
                for (var in names(var_info)) {
                    info <- var_info[[var]]
                    cat(sprintf("📌 %s:\n", var))
                    cat(sprintf("  🔧 Type: %s", info$type))
                    if (!is.null(info$units)) cat(sprintf(" | 📏 Units: %s", info$units))
                    cat("\n")
                    cat(sprintf("  📝 Description: %s\n", info$descr))
                    
                    if (var %in% names(data)) {
                        values <- data[[var]]
                        options <- if (is.numeric(values)) {
                                paste("from", round(min(values, na.rm = TRUE), 1), "to", round(max(values, na.rm = TRUE), 1))
                            } else {
                                vals <- unique(values)
                                lvl <- levels(values)
                                ifelse(
                                    is.null(lvl),
                                    paste(vals, collapse = ", "),
                                    paste(lvl, collapse = ", ")
                                )
                            }
                        cat(sprintf("  🔢 Values: %s\n", options))
                    } else {
                        cat("  ⚠️ Variable not found in 'data'\n")
                    }
                    cat("\n")
                }
            }

    # ANALYSIS

# Overview plotting
    # Select numeric data
        numeric_data <- data %>% 
            dplyr::select(dplyr::where(is.numeric), i.collection) %>% 
            dplyr::select(-i.completeness) %>%
            tidyr::pivot_longer(
                cols = -i.collection,
                names_to = "variable",
                values_to = "value"
            ) %>% 
            dplyr::rename(group = i.collection) %>% 
            dplyr::filter(!variable %in% excluded_vars)

    # Create boxplot for each numeric variable
        numeric_plots <- list()

        for (var in unique(numeric_data$variable)) {
            # fetch the var data
                df_var <- numeric_data %>% dplyr::filter(variable == var) %>% dplyr::filter(is.finite(value))
                info <- var_info[[var]]
            
            # count sample sizes
                counts <- df_var %>%
                    dplyr::group_by(group) %>%
                    dplyr::summarize(
                        n = sum(!is.na(value)),
                        y_min = ifelse(all(is.na(value)), NA, min(value, na.rm = TRUE)),
                        y_max = ifelse(all(is.na(value)), NA, max(value, na.rm = TRUE)),
                        .groups = "drop"
                    ) %>%
                    dplyr::mutate( # If all values фку identical or NA, offset to 0
                        range   = ifelse(!is.na(y_min) & y_min != y_max, y_max - y_min, 0),
                        label_y = y_min - 0.05 * range   # 5% below the min
                    )

            # Boxplot the variable
                p <- ggplot2::ggplot(df_var, ggplot2::aes(x = group, y = value)) +
                    ggplot2::geom_boxplot() +
                    ggplot2::geom_text(
                        data = counts,
                        aes(x = group, y = label_y, label = paste("n =", n)),
                        vjust = 1,
                        size = 3
                    ) +
                    ggplot2::labs(
                        title = var,
                        subtitle = stringr::str_wrap(info$descr, width = 60),
                        x = NULL,
                        y = "Value"
                    ) +
                    ggplot2::theme_minimal() +
                    coord_cartesian(clip = "off") +
                    theme(
                        axis.text  = element_text(size = 6),
                        axis.title = element_text(size = 6),
                        plot.title = element_text(size = 10),
                        plot.subtitle = element_text(size = 6),
                        plot.margin = unit(c(5, 5, 20, 5), "pt")
                    )
            # save the plot to the list
                numeric_plots[[var]] <- p
        }

    # Summarise non-numeric variables
        sum_tables <- sum_non_numeric(data, "i.completeness")
        
        non_numeric_plots <- list()

        for (var in names(sum_tables)) {
            # fetch the var data
                df <- sum_tables[[var]] %>%
                    tidyr::pivot_longer(
                        cols = -group,
                        names_to = "Option",
                        values_to = "Count"
                        )

                info <- var_info[[var]]

            # restore the factor levels
                if (var %in% names(data) && is.factor(data[[var]])) {
                    desired_levels <- levels(data[[var]])
                    df$Option <- factor(df$Option, levels = desired_levels)
                } else {
                    df$Option <- factor(df$Option, levels = sort(unique(df$Option)))
                }

            # create a grouped bar plot
                p <- ggplot2::ggplot(
                    df,
                    ggplot2::aes(x = group, y = Count, fill = Option)) +
                    ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
                    ggplot2::labs(
                        title = var,
                        subtitle = stringr::str_wrap(info$descr, width = 60),
                        x = NULL,
                        y = "Count"
                    ) +
                    ggplot2::scale_fill_viridis_d() +
                    ggplot2::theme_minimal() +
                    ggplot2::theme(
                        axis.text = ggplot2::element_text(size = 6),
                        axis.title = ggplot2::element_text(size = 6),
                        plot.title = ggplot2::element_text(size = 10),
                        plot.subtitle = ggplot2::element_text(size = 6),
                        axis.title.x = ggplot2::element_blank(),
                        legend.text = ggplot2::element_text(size = 8),
                        legend.title = ggplot2::element_text(size = 8)
                    )

                non_numeric_plots[[var]] <- p
        }

    # Save the results
        results$numeric_data <- numeric_data
        results$non_numeric_data <- sum_tables
        results$numeric_plots <- numeric_plots
        results$non_numeric_plots <- non_numeric_plots

    # Combine plots by prefix
        group_plots     <- non_numeric_plots[  grepl("group", names(non_numeric_plots)) ]
        other_plots <- non_numeric_plots[ !grepl("group", names(non_numeric_plots)) ]
        non_group_plots <- c(numeric_plots, other_plots)
        
        get_prefix <- function(name) sub("\\..*", "", name)

        plot_names <- names(non_group_plots)
        prefixes <- unique(get_prefix(plot_names))
        excluded_prefixes <- NULL
        prefixes <- setdiff(prefixes, excluded_prefixes)
        
        merged_plots <- list()


        if (!dir.exists(file.path(savepath, "overview"))) {
        dir.create(file.path(savepath, "overview"), recursive = TRUE)
        }



        for (prefix in prefixes) {
            plots_for_prefix <- non_group_plots[ sapply(plot_names, function(x) get_prefix(x) == prefix) ]
            file_name <- file.path(savepath, "overview", paste0(prefix, "_plots.png"))

            merged_plot <- gridExtra::arrangeGrob(grobs = plots_for_prefix, ncol = 4)

            #tiff(filename = file_name, width = 30, height = 20, units = "cm", res = 300)
            png(filename = file_name, width = 30, height = 20, units = "cm", res = 300)
            grid::grid.draw(merged_plot)
            dev.off()
            
            merged_plots[[prefix]] <- merged_plot
        }

       if (length(group_plots) > 0) {
            file_name <- file.path(savepath, "overview", "group_plots.png")
            merged_group_plot <- gridExtra::arrangeGrob(grobs = group_plots, ncol = length(group_plots))
            #tiff(filename = file_name, width = 30, height = 10, units = "cm", res = 300)
            png(filename = file_name, width = 30, height = 10, units = "cm", res = 300)
            grid::grid.draw(merged_group_plot)
            dev.off()
            }

        results$merged_plots <- merged_plots
        #if (talky) cat("💾 Files have been saved to", outpath)
        return(results)
}