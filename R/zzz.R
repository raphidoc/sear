# global reference to kaitaistruct (will be initialized in .onLoad)
kaitaistruct <- NULL

# helper function to skip tests if we don't have the 'kaitaistruct' module
skip_if_no_kaitaistruct <- function() {
  have_kaitaistruct <- reticulate::py_module_available("kaitaistruct")
  if (!have_kaitaistruct)
    testthat::skip("kaitaistruct not available for testing")
}

.onLoad <- function(lib, pkg) {
  reticulate::use_condaenv(condaenv = "py_sear", conda = "auto", required = NULL)

	# use superassignment to update global reference to kaitaistruct
	kaitaistruct <<- reticulate::import("kaitaistruct", delay_load = TRUE)

	# Source python binary parser
	#reticulate::source_python(system.file("py","hocr.py", package = "sear", mustWork = T))

	# Set token for mapbox (GL rendering)
	Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoicmFwaGlkb2MiLCJhIjoiY2wwaTg3b3B2MDA1ODNibXJ2NHpvOWxweiJ9.WPYL0Tww3y7djCY4D3jlnA')

	# Set MathJax path sor TeX rendering
	Sys.setenv('PLOTLY_MATHJAX_PATH' = '/home/raphael/MathJax')
}
