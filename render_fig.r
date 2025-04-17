## ---- renderfig ----
render_fig <- function(path) {
    # load settings
    library(knitr)
    source("settings.r")
    #if (!exists("plot_titles") || length(plot_titles) > 0) {
    #    stop("No plot titles were found in settings")
    #}
    if (!file.exists(path)) {
        stop("The file does not exist: ", path)
    }
    
    name <- gsub("\\..*", "", basename(path))   
    if (!name %in% names(plot_titles)) {
        stop("No figure description")
    }

    title <- sprintf("Fig. %d. %s", plot_titles[[name]]$order, plot_titles[[name]]$title)
    desc <- plot_titles[[name]]$details
    
    # Emit foldable HTML block (needs chunk with results='asis')
        cat(sprintf('<details open class = "foldable fold-show">\n'))
        cat(sprintf('<summary><strong>%s</strong></summary>\n\n', title))

      #  if (!is.null(desc) && nzchar(desc)) {
       #     cat(desc, "\n\n")
        #}

    # Include image
    #knitr::include_graphics(path)
    cat(sprintf('<img src="%s" style="max-width:100%%;"/>\n\n', path))
    cat("\n\n</details>\n")

}
