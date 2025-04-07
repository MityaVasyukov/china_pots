parse_bounds <- function(x) {
    dependencies <-  c("stringr", "dplyr")
    invisible(lapply(dependencies, load_pkg))
    
    nums <- stringr::str_extract_all(x, "\\d*\\.?\\d+")

    result <- lapply(seq_along(nums), function(i) {
        n <- as.numeric(nums[[i]])
        
        if (length(n) == 2) {
            n <- sort(n)
            lower <- n[1]
            upper <- n[2]
        } else if (length(n) == 1) {
            if (stringr::str_detect(x[i], "<")) {
                lower <- 0
                upper <- n
            } else if (stringr::str_detect(x[i], ">")) {
                lower <- n
                upper <- NA
            } else {
                lower <- upper <- n
            }
        } else {
            lower <- upper <- NA
        }

        dplyr::tibble(original = x[i], lower = lower, upper = upper)
    })

    return(dplyr::bind_rows(result))

}