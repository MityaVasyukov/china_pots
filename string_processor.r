
# Calculates and renders the string 'mean+-sd (n)''
string_processor <- function(x) {
    m <- round(mean(x, na.rm = TRUE), 1)
    s <- round(sd(x, na.rm = TRUE), 1)
    n <- sum(!is.na(x))
    paste0(m, " ± ", s, " (", n, ")")
}