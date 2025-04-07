
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

# drwaing a pot perimeter
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

# getting colors for the coloration zones
coloration <- function(df, site, period) {

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

    color_in <- col_by_sides(tmp, "IN")  # Pass "IN" directly
    color_out <- col_by_sides(tmp, "OUT")  # Pass "OUT" directly
    colors <- list(inside = color_in, outside = color_out)

    return(colors)
}

colors <- coloration(counts, "Gaoqingcaopo", "Zhou")


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
            
        for (i in seq_along(plgn$inside)) {
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

plot    
































































#x_right <- -rev(x_left)
#perimeter <- data.frame(x = c(x_left, x_right+x_left[1]*2), y = c(y, rev(y)))

ggplot() + 
        coord_fixed(ratio = 1) + 
        geom_path(data = perimeter, aes(x = x, y = y), linewidth=1) +
        geom_point(data = perimeter, aes(x = x, y = y)) #+
        #theme_void()



x_out <- c(0, 645, 645, 611, 574, 602, 597, 550, 475, 441, 300, 200, 0)
x_in <- -rev(x_out)
y <- -c(136, 136, 173, 194, 236, 443.8, 716, 1088, 1303, 1320, 1260, 1204, 1088)
perimeter <- data.frame(x = c(x_out, x_in), y = c(y, rev(y)))

lip_OUT = rbind(perimeter[1:3,], c(0, perimeter[3,2]))
neck_OUT = rbind(perimeter[3:5,], c(0, perimeter[5,2]), c(0, perimeter[3,2]))
shoulder_OUT = rbind(perimeter[5:6,], c(0, perimeter[6,2]), c(0, perimeter[5,2]))
body_OUT = rbind(perimeter[6:8,], c(0, perimeter[13,2]), c(0, perimeter[6,2]))
foot_OUT = rbind(perimeter[8:11,], c(perimeter[11,1]-50, perimeter[8,2]))
crotch_OUT = rbind(perimeter[11:13,], c(perimeter[11,1]-50, perimeter[8,2]))

lip_IN <- lip_OUT %>% mutate( x = -x )
neck_IN <- neck_OUT %>% mutate( x = -x )
shoulder_IN <- shoulder_OUT %>% mutate( x = -x )
body_IN <- body_OUT %>% mutate( x = -x )
foot_IN <- foot_OUT %>% mutate( x = -x )
crotch_IN <- crotch_OUT %>% mutate( x = -x )

polygons <- list(
    lip_OUT, neck_OUT, shoulder_OUT, body_OUT, foot_OUT, crotch_OUT,
    lip_IN, neck_IN, shoulder_IN, body_IN, foot_IN, crotch_IN)

#true colors will go here
colors <- counts %>%
  filter( period == "Shang") %>%
  arrange(desc(side), part) %>%
  pull(hex)
colors <- c(colors, "#ffffff") 
#rep(c("lightblue", "red", "yellow", "green", "purple", "cyan"), 2)


labels <- data.frame(
  x = rep(0,6),
  y = c(
        mean(perimeter[2,2], perimeter[3,2]),
        mean(perimeter[3,2], perimeter[5,2]),
        mean(perimeter[5,2], perimeter[6,2]),
        mean(perimeter[6,2], perimeter[8,2]),
        perimeter[10,2],
        perimeter[10,2]),
  label = unique(fltr$part)
)



plot_polygons <- function(plgn, col) {
    p <- ggplot() + 
        coord_fixed(ratio = 1) + 
        geom_path(data = perimeter, aes(x = x, y = y), linewidth=1) +
        geom_point(data = perimeter, aes(x = x, y = y)) #+
        #theme_void()
        
        #labs(x = "X Coordinate", y = "Y Coordinate", title = "Sequential Line Plot of Coordinates with Polygon") +
        
    for (i in seq_along(plgn)) {
        poly_data <- plgn[[i]]
        poly_color <- col[i]
        p <- p + geom_polygon(data = poly_data, aes(x = x, y = y), fill = poly_color, color = "black")
    }
    
    return(p)
}

plot <- plot_polygons(polygons, colors) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 3, lineend = "round") #+
    #geom_text(aes(label = labels), vjust = -1) 

plot









library(readxl)
library(dplyr)
library(tidyverse)
library(ggplot2)

setwd("d:/PROJECTS/2024-vessels")
df <- read_excel("Data.xlsx")

fltr <- df %>%
select(record_id, period, col_in_lip, col_in_neck, col_in_shoulder, col_in_body, col_out_lip, col_out_neck, col_out_shoulder, col_out_body) %>%
pivot_longer(cols = starts_with("col_"), names_to = "part", values_to = "state") %>%
mutate(side = ifelse(grepl("col_in_", part), "IN", "OUT"),
 part = gsub("col_(in|out)_", "", part)) %>%
filter(state %in% c("carb", "clear"))

desired_order <- c("lip", "neck", "shoulder", "body")

counts <- fltr %>%
group_by(period, side, part, state) %>%
summarise(count = n(), .groups = "drop") %>%
pivot_wider(names_from = state, values_from = count, values_fill = list(count = 0)) %>%
mutate(
total = carb + clear,
prop_carb = round(100 * carb / total, 2),
prop_clear = round(100 * clear / total, 2),
color = round(255 * (1 - prop_carb / 100), 0),
hex = sprintf("#%02x%02x%02x", color, color, color),
order = case_when(
part == "lip" ~ 1,
part == "neck" ~ 2,
part == "shoulder" ~ 3,
part == "body" ~ 4
),
bar_height = ifelse(part == "body", 3, 1)
) %>% 
mutate(part = factor(part, levels = c("lip", "neck", "shoulder", "body")),
order = factor(part, levels=1:4))

labels_df <- data.frame(
part = c("Body", "Shoulder", "Neck", "Lip"),
y = c(1.5, 3.5, 4.5, 5.5)
)
 
ggplot(counts, aes(x = side, y = bar_height, fill = hex)) +
geom_bar(stat = "identity", position = "stack", width = 0.5) +
facet_grid(rows = vars(period)) +
geom_text(data = counts, aes(x = side, y = bar_height, label = paste(carb,"/", clear)), 
position = position_stack(vjust = 0.5), size = 4, color = "white") +
geom_text(data = labels_df, aes(x = 1.5, y = y, label = part), inherit.aes = FALSE) +
scale_fill_identity() +
labs(x = NULL, y = NULL, title = "Coloration level of vessels (carb/clear)") +
theme_minimal() +
theme(
axis.text.x = element_text(size = 14),
axis.text.y = element_blank(),
axis.ticks = element_blank(),
panel.grid = element_blank(),
strip.text = element_text(size = 14),
plot.title = element_text(size = 20, hjust = 0.5)
)
