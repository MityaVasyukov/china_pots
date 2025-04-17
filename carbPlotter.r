## ---- carbplotter ----
carb_plotter <- function(mode = "by_site_and_period", gap = 250) {

    modes <- c("by_site_and_period", "by_site", "by_period")

    # loadings
        source("plot_carb_pattern.r")
        source("settings.r")
        source("load_pckgs.r")
        dependencies <- c("readxl", "dplyr", "tidyr", "ggplot2", "grid", "gridExtra", "purrr", "sysfonts", "showtext", "knitr")
        invisible(lapply(dependencies, load_pkg))
        sysfonts::font_add_google("Open Sans", "opensans")
        showtext::showtext_auto()
        grDevices::windowsFonts(opensans = windowsFont("Open Sans"))

        data <- readxl::read_excel(file.path(inpath, info_and_carb_data_file_name), na = "")

    # filtering data
        carb <- data %>%
            dplyr::select(
                record_id,
                site_name, period,
                dplyr::starts_with("col_")
            ) %>%
            tidyr::pivot_longer(
                cols = dplyr::starts_with("col_"),
                names_to = "part",
                values_to = "state"
            ) %>%
            dplyr::mutate(
                side = ifelse(grepl("col_in_", part), "IN", "OUT"),
                part = gsub("col_(in|out)_", "", part)
            ) %>%
            dplyr::filter(
                state %in% c("carb", "clear")
            )

    # drawing a pot perimeter
        x <- c(0, 373, 375, 368, 344,
            322, 316, 342, 368, 384,
            397, 399, 395, 384, 366,
            346, 289, 238, 179, 128,
            62, 0)

        y <- -c(0, 0, 17, 35, 53,
            77, 99, 152, 234, 357,
            514, 630, 785, 856, 873, 
            867, 818, 765, 715, 679,
            657, 648)

        perimeter_IN <- data.frame( x = x, y = y )
        perimeter_OUT <- data.frame( x = -x-gap*2, y = y )

    # setting coloration zones
        lip <- rbind( perimeter_IN[1:4,], c(0, perimeter_IN[4,2]) )
        neck <- rbind( perimeter_IN[4:7,], c(0, perimeter_IN[7,2]), c(0, perimeter_IN[4,2]) )
        shoulder <- rbind( perimeter_IN[7:9,], c(0, perimeter_IN[9,2]), c(0, perimeter_IN[7,2]) )
        body <- rbind( perimeter_IN[9:12,], c(0, perimeter_IN[12,2]), c(0, perimeter_IN[9,2]) )
        foot <- rbind( perimeter_IN[12:19,], c(perimeter_IN[19,1], perimeter_IN[12,2]) )
        crotch <- rbind( perimeter_IN[19:22,], c(perimeter_IN[22,1], perimeter_IN[12,2]), c(perimeter_IN[19,1], perimeter_IN[12,2]) )

        polygons_OUT <- list(
            lip = lip,
            neck = neck,
            shoulder = shoulder,
            body = body,
            foot = foot,
            crotch = crotch
            )

        mirror <- function(df) {
            df$x <- df$x *(-1)-gap*2
            return(df)
        }

        polygons_IN <- lapply(polygons_OUT, mirror)
        polygons <- list(inside = polygons_IN, outside = polygons_OUT)
        
    # text labels
        labels <- data.frame(
            x = c(
                rep(-gap, 6),
                mean( c(-x[15], -x[22]) - gap*2 ),
                mean( c(x[15], x[22]) )
                ),
            y = c(
                mean(c(y[2], y[4])),
                mean(c(y[4], y[7])),
                mean(c(y[7], y[9])),
                mean(c(y[9], y[12])),
                mean(c(y[15], y[19])),
                y[22],
                rep(y[1]+0.1*abs(abs(y[1])-abs(y[15])), 2)
                ),
            label = c( parts, "INSIDE", "OUTSIDE" )
        )

    # run carb plotter
        if (!mode %in% modes) {
            stop(sprintf("%s is not an option. Available cases:\n%s\n", mode, paste(modes, collapse = ", ")))
        } else {
            if (mode == "by_site_and_period") {
                site_by_period <- carb %>% dplyr::select(period, site_name) %>% dplyr::distinct()
                plots <- site_by_period %>%
                    purrr::pmap(function(period, site_name) {
                        tryCatch({
                            plot_carb_pattern(
                                df = carb,
                                poly = polygons,
                                site = site_name,
                                period = period,
                                x = x,
                                y = y,
                                perimeter_IN = perimeter_IN,
                                perimeter_OUT = perimeter_OUT,
                                labels = labels,
                                cex = cex)
                        }, error = function(e) {
                            message(sprintf("Error for site '%s' and period '%s': %s", site_name, period, e$message))
                            return(NULL)
                        })

                    })
            } else if (mode == "by_site") {
                sites <- carb %>% dplyr::select(site_name) %>% dplyr::distinct()
                plots <- sites %>%
                    purrr::pmap(function( site_name ) {
                        tryCatch({
                            plot_carb_pattern(
                                df = carb,
                                poly = polygons,
                                site = site_name,
                                period = period,
                                x = x,
                                y = y,
                                perimeter_IN = perimeter_IN,
                                perimeter_OUT = perimeter_OUT,
                                labels = labels,
                                cex = cex)
                        }, error = function(e) {
                            message(sprintf("Error for site '%s': %s", site_name, e$message))
                            return(NULL)
                        })
                    })
            } else {
                periods <- carb %>% dplyr::select(period) %>% dplyr::distinct()
                plots <- periods %>%
                    purrr::pmap(function( period ) {
                        tryCatch({
                            plot_carb_pattern(
                                df = carb,
                                poly = polygons,
                                site = site_name,
                                period = period,
                                x = x,
                                y = y,
                                perimeter_IN = perimeter_IN,
                                perimeter_OUT = perimeter_OUT,
                                labels = labels,
                                cex = cex)
                        }, error = function(e) {
                            message(sprintf("Error for period '%s': %s", period, e$message))
                            return(NULL)
                        })
                    })
            }
        }
        
    # Cook the plots
        #plots <- flatten(plots)
        g <- gridExtra::arrangeGrob(grobs = plots, ncol = 2, nrow = 2)
        label_grob <- textGrob(
            "% of carbonized (total clear and carb). Darker color means larger percentage of carbonized sherds;",
            x = 0.65, y = 0.02,
            just = c("right", "bottom"),
            gp = gpar(fontsize = 12)
        )
        na_grob <- textGrob(
            " cross-lining means NA ",
            x = 0.65, y = 0.02,
            just = c("left", "bottom"),
            gp = gpar(fontsize = 12, col = "red")
        )

        final_grob <- gridExtra::arrangeGrob(
            g, label_grob, na_grob,
            ncol = 1,
            heights = unit(c(0.9, 0.05, 0.05), "npc")
        )

        results <- list(data = NULL, plot = final_grob)
        return(results)
}