        col_by_sides <- function(tmp, side) {
            col <- tmp %>%
                filter(side == !!side) %>%
                    complete(part = parts, fill = list(hex = na_col, text = "NA (0)")) %>%
                        mutate(part = factor(part, levels = parts)) %>%
                            arrange(part) %>%
                                select(hex, text)
                
            return(col)
        }