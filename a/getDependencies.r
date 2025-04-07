getDependencies <- function(packages) {
    tryCatch({
        
        getMissingPckg <- function(pkg) {
            if (!requireNamespace(pkg, quietly = TRUE)) 
                { install.packages(pkg) } 
        }

        invisible(sapply(packages, getMissingPckg))
        invisible(lapply(packages, library, character.only = TRUE))
        if (length(packages) > 1) {
            cat(sprintf("\n\nAll necessary packages:\n'%s'\nhave been loaded\n\n\n", paste(packages, collapse = ", ")))
        } else {
            cat(sprintf("Package '%s'have been loaded\n", paste(packages)))
        }
    }, error = function(e) {
        message(sprintf("Error: %s", e$message))
        }
    )
}

