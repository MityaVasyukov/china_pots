buildCondition <- function(conditions) {
    condition_texts <- c()
    for (i in 1:nrow(conditions)) {
        text <- (paste(conditions$query_string[i], "~", conditions$value[i]))
        condition_texts <- c(condition_texts, text)
    }
    return(paste(condition_texts, collapse = ", "))
}