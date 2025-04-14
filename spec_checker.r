spec_checker <- function(in_path, spec_path) {
    # List mzML files
        mzml_list <- list.files(path = spec_path, pattern = "\\.mzML$", full.names = TRUE)
        if (length(mzml_list) == 0) stop("No mzML files were found in the folder")

    # loadings
        source("settings.r")
        source("load_pckgs.r")
        dependencies <-  c("readxl", "dplyr", "stringr")
        invisible(lapply(dependencies, load_pkg))

    # read metadata
        df <- readxl::read_excel(file.path(in_path, info_and_carb_data_file_name), na = "") %>%
            dplyr::ungroup() %>%
            dplyr::rename(
                site = site_name,
                id   = aux_id
            ) %>%
            dplyr::select(id, period, site)

    # process filenames
        mzml_tbl <- dplyr::tibble(path = mzml_list) %>%
            dplyr::mutate(
                filename = basename(mzml_list),
                prefix   = stringr::str_extract(filename, "^[A-Za-z]"),
                num      = stringr::str_extract(filename, "(?<=^[A-Za-z])\\d+"),
                postfix  = stringr::str_extract(filename, "(?<=\\d)(.*)(?=\\.mzML)"),
                id       = paste0(prefix, num)
            ) %>%
            dplyr::select(path, id)

    # Check filenames
        missing_ids <- setdiff(mzml_tbl$id, df$id)
        if (length(missing_ids) != 0) warning("Following files were not matched: ", paste(missing_ids, collapse = ", "))

    # Merge metadata and file information:
        data <- inner_join(df, mzml_tbl, by = "id")
        if (nrow(data) == 0) stop("No filename matches the metadata ids.")
        cat("Number of matched mzML files:", nrow(data), "\n")

    return(data)
}