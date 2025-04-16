# Path Setting
wd <- getwd()  # Set the working directory manually if launching in R GUI

# Load Dependencies
source("load_pckgs.r")  # Load the library loader function
source("settings.r")    # Load the file with all static parameters set
source("plot_saver.r")

# Analysis

# Carbonization Pattern Plotter
source("carbPlotter.r")
carb <- carb_plotter(mode = "by_site_and_period", gap = 250)
plot_saver(carb$plot, outpath, "carb_pattern.png", 13.3, 10.7, 300)

# Dry or Wet Summaries
source("dryOrWet.r")
drywet <- dryOrWet("by_site_and_period")
plot_saver(drywet$plot, outpath, "dry_wet.png", 5, 5, 300)

#  Overviewer: Generate Overview Plots
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


# Spectral analysis
source("spec_checker.r")

spec_index <- spec_checker(inpath, path) # checks the files and returns the list of verified paths & metadata
print(spec_index)
source("spec_overviewer.r")
specovrw <- spec_overviewer(spec_index, 50) # provides a '3d' visualization of GCMS data for a given data file
plot_saver(specovrw$plot, outpath, "spectrum_overview.png", 10, 10, 100)

source("ticsnpeaks.r")
e <- ticsnpeaks(spec_index, 3) # fetches the tics for all spectra and the list of outstanding peaks
summary(e)
unlist(lapply(e$tics, nrow))
unlist(lapply(e$peaks, nrow))

source("process_tics.r")
ticz <- process_tics(data = e, index_tbl = spec_index, only_high_tic = T)
plot_saver(ticz$plot, outpath, "tics.png", 10, 10, 100)