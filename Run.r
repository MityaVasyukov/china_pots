###   SETTINGS AND LOADINGS   ###
# Path settings
wd <- getwd()
# Load dependencies
source("load_pckgs.r")
source("settings.r")
source("carbPlotter.r")
source("dryOrWet.r")
source("overviewer.r")
source("pca_analysis.r")
source("lda_analysis.r")
source("ran_for_analysis.r")
source("cluster_analysis.r")
source("means_clust_analysis.r")

load_pkg("readxl")
load_pkg("dplyr")

# Load data
raw_df <- read_excel(file.path(inpath, info_and_carb_data_file_name), na = "")

###   ANALYSIS   ###
# Run carbonization pattern plotter
carb_plotter(raw_df)

# Run dry/wet analysis
dryOrWet(raw_df, "by_site_and_period")

# Overview plotter through all the data
# prepare the data

# Run overviwer
source("overviewer.r")
r <- overviewer(inpath, outpath, talky =F)

summary(r)
print(r$numeric_plots$m.rim)
lapply(r$numeric_plots, print)
invisible(lapply(list.files(outpath, pattern = "\\.tiff$", full.names = TRUE), browseURL))


source("pca_analysis.r")
pca_analysis(r$data, "i.collection")
pca_analysis(r$data, "i.period")
pca_analysis(r$data, "i.site")

source("lda_analysis.r")
lda_analysis(r$data, "i.collection")


# Spectral analysis
path <- "p:/2025-vessels/spectra/mz"
source("spec_checker.r")
source("spec_overviewer.r")
source("ticsnpeaks.r")

spec_index <- spec_checker(inpath, path) # checks the files and returns the list of verified paths & metadata
spec_overviewer(spec_index, 50) # provides a '3d' visualization of GCMS data for a given data file
e <- ticsnpeaks(spec_index, 3) # fetches the tics for all spectra and the list of outstanding peaks
summary(e)
unlist(lapply(e$tics, nrow))
unlist(lapply(e$peaks, nrow))

source("process_tics.r")
process_tics(data = e, index_tbl = spec_index, only_high_tic = T)



source("spectra.r")

