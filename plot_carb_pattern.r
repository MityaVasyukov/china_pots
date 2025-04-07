# function generating a single plot, inside+outside
plot_carb_pattern <- function(df, poly, site = NULL, period = NULL,
                                x, y, perimeter_IN, perimeter_OUT, labels,
                                cex = 10, gap = 250, na_col = "#b8241a") {
    # loadings
        period_colors <- list( "Shang" = "blue", "Zhou" = "aquamarine" ) # color settings of periods
        source("col_by_sides.r")
        source("load_pckgs.r")
        dependencies <- c("dplyr", "tidyr", "ggplot2", "ggpattern")
        invisible(lapply(dependencies, load_pkg))

    if (!is.null(site) && !is.null(period)) {
        if (nrow(df %>% dplyr::filter(site_name == {{site}} & period == {{period}})) == 0) {
            plot <- ggplot2::ggplot()
            return(plot)
            stop("No site, no period")
            #! just stop("Error : no data for plotting")
        } else {
            counts <- df %>%
                dplyr::filter(site_name == !!site & period == !!period) %>%
                dplyr::group_by(site_name, period, side, part, state) %>%
                dplyr::summarise(count = n(), .groups = "drop") %>%
                tidyr::pivot_wider(names_from = state, values_from = count, values_fill = list(count = 0)) %>%
                dplyr::mutate(
                    total = carb + clear,
                    prop_carb = round(100*carb / total, 2),
                    prop_clear = round(100*clear / total, 2),
                    color = round(255 * (1 - prop_carb / 100), 0),
                    hex = sprintf("#%02x%02x%02x", color, color, color),
                    part = factor(part, levels = parts),
                    period = factor(period, levels = c("Shang", "Zhou"))  
                ) %>%
                dplyr::arrange(period, part)

            title_text <- sprintf(
                "%s, <span style='color:%s;'>%s</span> period",
                site,
                period_colors[[period]],
                period
            )
        }
    } else if (!is.null(site) && is.null(period)) {
        counts <- df %>%
            dplyr::select( -period ) %>%
            dplyr::filter(site_name == !!site) %>%
            dplyr::group_by(site_name, side, part, state) %>%
            dplyr::summarise(count = n(), .groups = "drop") %>%
            tidyr::pivot_wider(
                names_from = state,
                values_from = count,
                values_fill = list(count = 0)
            ) %>%
            dplyr::mutate(
                total = carb + clear,
                prop_carb = round(100*carb / total, 2),
                prop_clear = round(100*clear / total, 2),
                color = round(255 * (1 - prop_carb / 100), 0),
                hex = sprintf("#%02x%02x%02x", color, color, color),
                part = factor(part, levels = parts)  
            ) %>%
            dplyr::arrange(site_name, part)

        title_text <- sprintf("%s, any period", site)
    } else if (is.null(site) && !is.null(period)) {
        counts <- df %>%
            dplyr::select( -site_name ) %>%
            dplyr::filter( period == !!period ) %>%
            dplyr::group_by( period, side, part, state ) %>%
            dplyr::summarise( count = n(), .groups = "drop" ) %>%
            tidyr::pivot_wider(names_from = state, values_from = count, values_fill = list(count = 0)) %>%
            dplyr::mutate(
                total = carb + clear,
                prop_carb = round(100*carb / total, 2),
                prop_clear = round(100*clear / total, 2),
                color = round(255 * (1 - prop_carb / 100), 0),
                hex = sprintf("#%02x%02x%02x", color, color, color),
                part = factor(part, levels = parts),
                period = factor(period, levels = c("Shang", "Zhou"))  
            ) %>%
            dplyr::arrange(period, part)

        title_text <- sprintf(
            "All sites, <span style='color:%s;'>%s</span> period",
            period_colors[[period]],
            period
        )
    }

    counts <- counts %>%
        dplyr::select(side, part, hex, carb, clear) %>%
        dplyr::mutate(text = paste0(round(100*carb/(carb+clear)),"% (", carb + clear, ")")) %>%
        dplyr::arrange(side, part)

    color_in <- col_by_sides(counts, "IN")
    color_out <- col_by_sides(counts, "OUT")
    colors <- list(inside = color_in, outside = color_out)

    numbers <- data.frame(
        number = c(colors$inside$text, colors$outside$text),
        x = c(
            -rep(( 1.9*gap ), 4),
            -x[17]-1.9*gap,
            -1.9*gap,
            -rep(( 0.1*gap ), 4),
            x[17]-0.2*gap,
            -0.1*gap
            ),
        y = rep(c(
            mean(c(y[2], y[4])),
            mean(c(y[4], y[7])),
            mean(c(y[7], y[9])),
            mean(c(y[9], y[12])),
            y[17],
            y[22]
            ), 2
        )       
    )

    p <- ggplot2::ggplot()

    for (i in seq_along(poly$inside)) {
        poly_data <- poly$inside[[i]]
        poly_color <- colors$inside$hex[i]

        if (poly_color == na_col) {
            p <- p + ggpattern::geom_polygon_pattern(
                data = poly_data, ggplot2::aes(x = x, y = y, pattern = "diagonal"),
                pattern_fill = "transparent",
                pattern_density = 1,
                pattern_angle = 45,
                fill = "transparent",
                color = "black",
                show.legend = FALSE
            )

        } else {
            p <- p + ggplot2::geom_polygon(
                data = poly_data, ggplot2::aes(x = x, y = y),
                fill = poly_color,
                color = "black"
            )
        }
    }

    for (i in seq_along(poly$outside)) {
        poly_data <- poly$outside[[i]]
        poly_color <- colors$outside$hex[i]

        if (poly_color == na_col) {
            p <- p + ggpattern::geom_polygon_pattern(
                data = poly_data,
                ggplot2::aes(x = x, y = y, pattern = "diagonal"),
                pattern_fill = "transparent",
                pattern_density = 1,
                pattern_angle = 45,
                fill = "transparent",
                color = "black",
                show.legend = FALSE
            )

        } else {
            p <- p + ggplot2::geom_polygon(
                data = poly_data, ggplot2::aes(x = x, y = y),
                fill = poly_color,
                color = "black"
            )
        }
    }

    plot <-p +
        ggplot2::coord_fixed(ratio = 1) + 
        ggplot2::geom_path(
            data = perimeter_OUT,
            ggplot2::aes(x = x, y = y),
            linewidth = 1
        ) +
        ggplot2::geom_path(
            data = perimeter_IN,
            ggplot2::aes(x = x, y = y),
            linewidth = 1
            ) +
        ggplot2::labs(x = NULL, y = NULL, title = title_text) +
        ggplot2::geom_segment(
            ggplot2::aes(
                x = 0,
                y = y[1]+0.1*abs(abs(y[1])-abs(y[22])),
                xend = 0,
                yend = y[22]-0.1*abs(abs(y[1])-abs(y[22]))
                ),
            linetype = "dashed",
            linewidth = 1.5,
            lineend = "round"
            ) +
        ggplot2::geom_segment(
            ggplot2::aes(
                x = -gap*2,
                y = y[1]+0.1*abs(abs(y[1])-abs(y[22])),
                xend = -gap*2,
                yend = y[22]-0.1*abs(abs(y[1])-abs(y[22]))
                ),
            linetype = "dashed",
            linewidth = 1.5,
            lineend = "round"
            ) +
        ggplot2::geom_text(
            data = labels[7:8,],
            ggplot2::aes(x = x, y = y, label = label),
            size = 0.5*cex
            ) +
        ggplot2:: geom_text(
            data = numbers[1:6,],
            ggplot2::aes(x = x, y = y, label = number),
            size=0.4*cex,
            hjust = 0,
            nudge_x = -0.2
            ) +
        ggplot2::geom_text(
            data = numbers[7:12,],
            ggplot2::aes(x = x, y = y, label = number),
            size=0.4*cex,
            hjust = 1,
            nudge_x = 0.2
            ) +
        ggplot2::xlim(-900, 400) +
        hrbrthemes::theme_ipsum(base_family = "opensans") +
        ggplot2::theme(
            plot.title = ggtext::element_markdown(size = 2*cex, face = "bold", hjust = 0.5),
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_blank()
            )

    return(plot)
}