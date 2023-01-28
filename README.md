
# sear <a href=''><img src='inst/app/www/hex_sear.png' align="right" height="139" /></a>
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7459254.svg)](https://doi.org/10.5281/zenodo.7459254)
  <!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
  <!-- badges: end -->
  <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/raphidoc/sear/branch/main/graph/badge.svg)](https://app.codecov.io/gh/raphidoc/sear?branch=main)
  <!-- badges: end -->

Web-based reactive program for ocean optic data management and processing.

## Overview

The sear software is written in [shiny](https://github.com/rstudio/shiny) and was primarily designed to manage and process data for the [FORSCE SeaDoo project](https://ldgizc.uqar.ca/Web/infrastructures-et-equipements/forsce#forsce-english).
It allow to load, parse, display, process and save the data. Permanent data storage is done with an SQLite database for each project.

Along it's development the idea come that it could be more, something like a platform to manage and process ocean optic data coming from any numbers of instruments and platform. This could be achieve by defining the architecture common to all ocean optic projects, the specifics of each instrument and platform and the method to be applied upon them. It would improve the efficiency of the analysts and give a solution to common problems faced with data management, processing and quality control.

![](https://youtu.be/U6CH2Ictvo4)

## Features

### Settings
* Choose spectral resolution for HOCR output.
* Enter custom values for HOCR positions in water, distance from the surface and distance between the instruments.

### Load and parse
* Files `.txt` and `.bin` from MTE data logger.
* BioSonic csv exported by VisualAquatic.
* Calibration files for HOCR, SBE19. SBE18, SBE43, SeaOWL, BBFL2.
* Use longitude (`Lon`), latitude (`Lat`) and `DateTime` from GPS device (Applanix for the SeaDoo) to create a `MainLog`.
* Create a data synthesis, presence or absence of said instrument at a specific time in the `MainLog`.

### Filters

* Instruments combination based on the data synthesis
* Date range
* Time range
* Solar azimuth relative to the boat heading (to asses self shadow effect on AOPs)
* Watercraft speed

### Display

* Map created with plotly and Mapbox display raw data points and discrete observation with their spatial buffer
* Raw data point can be selected and processed to L1b to create a discrete observation
* Discrete observation can be clicked on to display the associated data
* When raw data is selected the associated solar azimuth relative to the boat is displayed in a polar plot

### Processing

* Human QC of L1b data, analyst can click on the data to shift it between green (1 = good) and red (0 = bad). Data that have the flag QC flag 0 is removed from further analysis.
* Process L2 to summarize (mean) non spectral data. For the HOCRs, AOPs are computed.
* Loess interpolation and smoothing for AOPs.

### Save

* Once all instrument are processed, `Save` send the discrete observation L1b and L2 data to the SQL backend.
* If `Save` is hit for a UUID that is already present (reprocessing for example) the data corresponding to this UUID is updated.
* `Delete` to delete an observation from the database.

## Instalation

The software can be installed from GitHub. 
To simplify the installation process, a python virtual env is created to use the python binary parser created with [Kaitai Struct](https://kaitai.io/)
  
## Code of Conduct
  
  Please note that the sear project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
