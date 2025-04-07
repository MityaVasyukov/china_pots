# Calculates a summary table per each non-numeric variable
sum_non_numeric <- function(data, numeric_to_add) {
   ## dependencies
    source("load_pckgs.r")
    dependencies <-  c("dplyr", "tidyr")
    invisible(lapply(dependencies, load_pkg))

   
    summary_list <- list()

    non_numeric_cols <- setdiff(names(data)[!sapply(data, is.numeric)], "period_site")
    excluded_vars <- c("id", "i.site", "i.period", "i.collection")
    non_numeric_cols <- c(setdiff(non_numeric_cols, excluded_vars), numeric_to_add)

    # Loop over each non-numeric variable and make a summary table
    for (var in non_numeric_cols) {
        t <- as.data.frame(table(
            option = data[[var]],
            group = data$i.collection
        )) %>%
        tidyr::pivot_wider(
            names_from = option,
            values_from = Freq,
            values_fill = list(Freq = 0)
        ) %>%
        dplyr::arrange(group)

        summary_list[[var]] <- t
        }
        
    return(summary_list)
    }
