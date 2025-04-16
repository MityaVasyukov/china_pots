## ---- loadpkg ----
load_pkg <- function(pkg) {
    # Input validation
        if (!is.character(pkg) || length(pkg) != 1) {
            stop("Parameter 'pkg' must be a single character string.")
        }
    
    # Check if the package is installed; if not, attempt installation.
        if (!requireNamespace(pkg, quietly = TRUE)) {
            tryCatch({
                message("Installing ", pkg)
                install.packages(pkg, dependencies = TRUE, repos = "https://cloud.r-project.org", quiet = TRUE)
            }, error = function(e) {
                stop(sprintf("Installation failed for package '%s': %s", pkg, e$message))
            })
        }
        
    # Attempt to load the package.
        if (!paste0("package:", pkg) %in% search()) {
            tryCatch({
                #message("Loading ", pkg)
                suppressPackageStartupMessages(library(pkg, character.only = TRUE))
            }, error = function(e) {
                stop(sprintf("Loading failed for package '%s': %s", pkg, e$message))
            })
        }
    
    invisible(pkg)
}
