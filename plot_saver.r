## ---- plotsaver ----
plot_saver <- function(object, path, width, height, dpi) {
  source("load_pckgs.r")
  dependencies <- c("ggplot2", "knitr")
  
  # Save the plot using ggsave
  ggsave(
    filename = path,
    plot = object,
    width = width,
    height = height,
    dpi = dpi
  )
  
#  # Generate HTML code for clickable full-width image
#  html_code <- paste0(
#    '<a href="', full_path, 
#    '" data-lightbox="gallery">',
#    '<img src="', full_path, 
#    '" class="full-width" alt="Plot image" />',
#    '</a>'
#  )
#  
#  knitr::asis_output(html_code)
}
