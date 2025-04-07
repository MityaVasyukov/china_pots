#' Quietly Load and Install Package if Necessary
#'
#' This function checks whether a specified package is installed. If it is not installed, the function attempts to install it from CRAN.
#' Afterwards, it loads the package while suppressing startup messages.
#'
#' @param pkg A character string specifying the name of the package to load.
#'
#' @return Invisibly returns the name of the package after loading.
#'
#' @details The function first checks for the package using \code{requireNamespace()}. If not available, it installs the package from the CRAN mirror
#' \url{https://cloud.r-project.org} with all dependencies. Once installed, it loads the package using \code{library()} with suppressed startup messages.
#'
#' @examples
#' \dontrun{
#'   # Load the 'ggplot2' package, installing it first if needed.
#'   load_pkg("ggplot2")
#' }
#'
#' @export
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
                message("Loading ", pkg)
                suppressPackageStartupMessages(library(pkg, character.only = TRUE))
            }, error = function(e) {
                stop(sprintf("Loading failed for package '%s': %s", pkg, e$message))
            })
        }
    
    invisible(pkg)
}
