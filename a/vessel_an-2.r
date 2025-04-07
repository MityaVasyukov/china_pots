
library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)

# settings
#setwd("d:/PROJECTS/2024-vessels")
setwd("c:/Users/Public/Documents/2024-vessels")
df <- read_excel("Data.xlsx")
parts <- c("lip", "neck", "shoulder", "body", "foot", "crotch")
gap <- 75

# data filtering
fltr <- df %>%
    select(record_id, site_name, period, starts_with("col_")) %>%
        pivot_longer(cols = starts_with("col_"), names_to = "part", values_to = "state") %>%
            mutate(
                side = ifelse(grepl("col_in_", part), "IN", "OUT"),
                part = gsub("col_(in|out)_", "", part)
            ) %>%
                filter(state %in% c("carb", "clear"))

# data summarizing
counts <- fltr %>%
    group_by(site_name, period, side, part, state) %>%
        summarise(count = n(), .groups = "drop") %>%
            pivot_wider(names_from = state, values_from = count, values_fill = list(count = 0)) %>%
                mutate(
                    total = carb + clear,
                    prop_carb = round(100*carb / total, 2),
                    prop_clear = round(100*clear / total, 2),
                    color = round(255 * (1 - prop_carb / 100), 0),
                    hex = sprintf("#%02x%02x%02x", color, color, color),
                    part = factor(part, levels = parts)  
                ) %>%
                    arrange(part)

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

polygons_OUT <- list( lip, neck, shoulder, body, foot, crotch )

mirror <- function(df) {
    df$x <- df$x *(-1)-gap*2
    return(df)
}

polygons_IN <- lapply( polygons_OUT, mirror )

polygons <- list( inside = polygons_IN, outside = polygons_OUT)

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
        rep(y[15] - gap, 2)
    ),
    label = c( parts, "INSIDE", "OUTSIDE" )
)

################ DYNAMIC ######################

plot_vessel <- function(df, site, period) {

# Check if the number of rows after filtering by site and period is greater than 0
    if (nrow(df %>% filter(site_name == site & period == period)) == 0) {
        plot<- ggplot()
        return(plot)  # Return NULL or handle the case when there are no rows
    } else {


plot_vessel(counts, "Gaoqingcaopo", "Zhou")

    col_by_sides <- function(tmp, side) {
        col <- tmp %>%
            filter(side == !!side) %>%
                complete(part = parts, fill = list(hex = "#FF0000", text = "0 / 0")) %>%
                    mutate(part = factor(part, levels = parts)) %>%
                        arrange(part) %>%
                            select(hex, text)
        
        return(col)
    }

    tmp <- df %>%
        filter(site_name == {{site}} & period == {{period}}) %>%
            select(side, part, hex, carb, clear) %>%
                mutate(text = paste(carb, "/", clear)) %>%
                    arrange(side, part)

    color_in <- col_by_sides(tmp, "IN")
    color_out <- col_by_sides(tmp, "OUT")
    colors <- list(inside = color_in, outside = color_out)

    numbers <- data.frame(
        number = c(colors$inside$text, colors$outside$text),
        x = c(
            -rep((max(x) + 3*gap), 5),
            -x[21]-2*gap,
            rep((max(x) + gap), 5),
            x[21]
        ),
        y = rep(c(
                mean(c(y[2], y[4])),
                mean(c(y[4], y[7])),
                mean(c(y[7], y[9])),
                mean(c(y[9], y[12])),
                mean(c(y[12], y[15])),
                y[19]
                ),
        2)       
    )

    vesplot <- function(plgn, col) {
        p <- ggplot() + 
            coord_fixed(ratio = 1) + 
            geom_path(data = perimeter_OUT, aes(x = x, y = y), linewidth=1) +
            geom_path(data = perimeter_IN, aes(x = x, y = y), linewidth=1) +
            labs(x = "X Coordinate", y = "Y Coordinate", title = "Sequential Line Plot of Coordinates with Polygon") +
            theme_void()
            #geom_point(data = perimeter, aes(x = x, y = y)) +
                
            for (i in seq_along(polygons$inside)) {
                poly_data <- plgn$inside[[i]]
                poly_color <- col$inside$hex[i]
                p <- p + geom_polygon(data = poly_data, aes(x = x, y = y), fill = poly_color, color = "black")
            }
            
            for (i in seq_along(plgn$outside)) {
                poly_data <- plgn$outside[[i]]
                poly_color <- col$outside$hex[i]
                p <- p + geom_polygon(data = poly_data, aes(x = x, y = y), fill = poly_color, color = "black")
            }
            
        return(p)
    }

    plot <- vesplot(polygons, colors) +
    #facet_grid(rows = vars(period)) +
        geom_segment(aes(x = 0, y = y[1]+gap, xend = 0, yend = y[22]-gap), linetype = "dashed", linewidth = 1.5, lineend = "round") +
        geom_segment(aes(x = -gap*2, y = y[1]+gap, xend = -gap*2, yend = y[22]-gap), linetype = "dashed", linewidth = 1.5, lineend = "round") +
        geom_text(data = labels, aes(x = x, y = y, label = label)) +
        geom_text(data = numbers, aes(x = x, y = y, label = number) )

    return(plot)    


}
}

#plot_vessel(counts, "Gaoqingcaopo", "Shang" )

sites <- unique(counts$site_name)
periods <- unique(counts$period)

plots <- list()

# Loop through each unique combination of site and period
for (site in sites) {
  for (period in periods) {
  print(c(site, period))
     
   
    plot <- plot_vessel(counts, site, period)
    plots <- c(plots, list(plot))
  }
}

plot_vessel(counts, site, period)



combined_plot <- marrangeGrob(plots, nrow = length(sites), ncol = length(periods))

# Display the combined plot
grid::grid.newpage()
grid::grid.draw(combined_plot)

