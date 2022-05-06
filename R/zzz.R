# global reference to kaitaistruct (will be initialized in .onLoad)
kaitaistruct <- NULL

.onLoad <- function(lib, pkg) {
	# use superassignment to update global reference to kaitaistruct
	kaitaistruct <<- reticulate::import("kaitaistruct", delay_load = TRUE)

	# Source python binary parser
	#reticulate::source_python(system.file("py","hocr.py", package = "sear", mustWork = T))

	# Set token for mapbox (GL rendering)
	Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoicmFwaGlkb2MiLCJhIjoiY2wwaTg3b3B2MDA1ODNibXJ2NHpvOWxweiJ9.WPYL0Tww3y7djCY4D3jlnA')

	# Set MathJax path sor TeX rendering
	Sys.setenv('PLOTLY_MATHJAX_PATH' = '/home/raphael/MathJax')
}
