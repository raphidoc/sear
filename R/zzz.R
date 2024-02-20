#' @useDynLib sear
#' @importFrom Rcpp evalCpp

# global reference to kaitaistruct (will be initialized in .onLoad)
kaitaistruct <- NULL

# helper function to skip tests if we don't have the 'kaitaistruct' module
skip_if_no_kaitaistruct <- function() {
  have_kaitaistruct <- reticulate::py_module_available("kaitaistruct")
  if (!have_kaitaistruct) {
    testthat::skip("kaitaistruct not available for testing")
  }
}

.onLoad <- function(lib, pkg) {
  packageStartupMessage(
    "sear v",
    utils::packageDescription("sear",
      fields = "Version"
    )
  )

  # reticulate::use_condaenv(condaenv = "py_sear", conda = "auto", required = NULL)

  if (!"reticulate" %in% installed.packages()) {
    install.packages("reticulate")
  }

  if (!dir.exists(reticulate::miniconda_path())) {
    reticulate::install_miniconda(update = T)
  }

  if (!any(stringr::str_detect(
    reticulate::conda_list(
      conda = file.path(reticulate::miniconda_path(), "bin", "conda")
    )$name,
    "pysear"
  ))
  ) {
    reticulate::conda_create(
      envname = "pysear",
      packages = c("kaitaistruct", "python-kaleido", "plotly"),
      forge = TRUE,
      environment = NULL,
      conda = file.path(reticulate::miniconda_path(), "bin", "conda"),
      python_version = miniconda_python_version()
    )
  }

  # reticulate::conda_install('pysear', 'kaitaistruct')
  # reticulate::conda_install('pysear', 'python-kaleido')
  # reticulate::conda_install('pysear', 'plotly', channel = 'plotly')

  # reticulate::conda_install('pysear', 'orca')

  reticulate::use_miniconda("pysear", required = T)

  # Workaround for save_image() https://github.com/plotly/plotly.R/issues/2179
  reticulate::py_run_string("import sys")


  # reticulate::virtualenv_create("pysear")
  # reticulate::virtualenv_install("pysear", "kaitaistruct")
  ## reticulate::virtualenv_install("pysear", "kaleido")
  # reticulate::virtualenv_install("pysear", 'plotly', channel = 'plotly')
  # reticulate::use_virtualenv("pysear", required = T)

  # use superassignment to update global reference to kaitaistruct
  kaitaistruct <<- reticulate::import("kaitaistruct", delay_load = TRUE)

  # Source python binary parser
  # reticulate::source_python(system.file("py","hocr.py", package = "sear", mustWork = T))

  # Set token for mapbox (GL rendering)
  Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1IjoicmFwaGlkb2MiLCJhIjoiY2xudXd6cm5zMGY4ODJxbXUzZzh1MnAzOSJ9.L7OyTtYAoE5fyL_1m4b4NQ")

  # Set MathJax path sor TeX rendering
  Sys.setenv("PLOTLY_MATHJAX_PATH" = "/home/raphael/MathJax")
}

# .onAttach <- function(lib, pkg) {
#   packageStartupMessage(
#     "sear v",
#     utils::packageDescription("sear",
#       fields = "Version"
#     )
#   )
# }
