#' ---
#' title: "Carbonization Analysis"
#' author: "Your Name"
#' date: "`r format(Sys.Date(), '%B %d, %Y')`"
#' output:
#'   html_document:
#'     toc: true           # Enables a table of contents
#'     toc_depth: 2        # Sets header depth for the TOC
#'     number_sections: true
#'     theme: cerulean     # Uses the Cerulean Bootstrap theme
#'     includes:
#'      in_header: custom_header.html
#' params:
#'   # You can add custom parameters here if needed
#' ---

## ---- setup, include=FALSE
knitr::opts_chunk$set(
  fig.cap = "",        # Default figure caption is empty
  fig.width = 15,       # Default figure width
  fig.height = 15,      # Default figure height
  dpi = 300,           # Resolution for the figures
  echo = FALSE,        # Hide the R code in the final document
  warning = FALSE,     # Suppress warnings
  message = FALSE      # Suppress messages
)

#' # Path Setting
wd <- getwd()  # Set the working directory manually if launching in R GUI

#' # Load Dependencies
source("load_pckgs.r")  # Load the library loader function
source("settings.r")    # Load the file with all static parameters set

#' # Analysis

#' ## 1. Carbonization Pattern Plotter
#' The function loads the raw data from `info_and_carb_data_file_name` (parameter set in settings.r) 
#' and presents mean carbonisation patterns using the schemes "by_site_and_period", "by_site", or "by_period".
#' The `gap` parameter controls the aesthetics.
source("carbPlotter.r")
carb <- carb_plotter(mode = "by_site_and_period", gap = 250)
plot_saver(carb$plot, outpath, "carb_pattern.png", 13.3, 10.7, 300)

#' ## 2. Dry or Wet Summaries
#' The function loads the raw data from `info_and_carb_data_file_name` (parameter set in settings.r) 
#' and performs calculations for dry/wet classification based on conditions defined in `dry_wet_conditions` (set in settings.r).
source("dryOrWet.r")
drywet <- dryOrWet("by_site_and_period")
plot_saver(drywet$plot, outpath, "dry_wet.png", 5, 5, 300)

#' ## 3. Overviewer: Generate Overview Plots
#' Runs the overviewer function to generate additional plots and embeds them into the document.
source("overviewer.r")
r <- overviewer(inpath, outpath, talky = FALSE)
overvw_plots <- list.files(file.path(outpath, "overview"), full.names = TRUE)
imgs_markdown <- paste0("![](", overvw_plots, ")", collapse = "\n\n")
asis_output(imgs_markdown)
# Generate clickable image HTML tags
imgs_html <- paste0(
  '<a href="', overvw_plots, 
  '" data-lightbox="gallery">',
  '<img src="', overvw_plots, 
  '" class="full-width" />',
  '</a>'
  , collapse = "\n\n"
)
asis_output(imgs_html)


#' ## 3. Spectral analysis
source("spec_checker.r")

spec_index <- spec_checker(inpath, path) # checks the files and returns the list of verified paths & metadata
print(spec_index)



source("spec_overviewer.r")

specovrw <- spec_overviewer(spec_index, 50) # provides a '3d' visualization of GCMS data for a given data file

plot_saver(specovrw$plot, outpath, "spectrum_overview.png", 16, 16, 300)

source("ticsnpeaks.r")
e <- ticsnpeaks(spec_index, 3) # fetches the tics for all spectra and the list of outstanding peaks
summary(e)
unlist(lapply(e$tics, nrow))
unlist(lapply(e$peaks, nrow))

source("process_tics.r")
ticz <- process_tics(data = e, index_tbl = spec_index, only_high_tic = T)
plot_saver(ticz$plot, outpath, "tics.png", 16, 16, 300)

