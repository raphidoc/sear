#!/bin/bash
# Ask user for sudo password (to be used when needed)
read -s -p "Enter Password for sudo: " sudoPW

sudo apt update
sudo apt -y install nginx


# add this line in the http{} 
# client_max_body_size 1G;
sudo nano /etc/nginx/nginx.conf

sudo systemctl restart nginx
sudo systemctl status nginx

# R setup
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
gpg --keyserver keyserver.ubuntu.com --recv-key E298A3A825C0D65DFD57CBB651716619E084DAB9
gpg -a --export E298A3A825C0D65DFD57CBB651716619E084DAB9 | sudo apt-key add -

sudo apt-get update
sudo apt-get install r-base

# System apt and R CRAN library integration
wget https://raw.githubusercontent.com/eddelbuettel/r2u/master/inst/scripts/add_cranapt_jammy.sh
chmod +x add_cranapt_jammy.sh
./add_cranapt_jammy.sh

sudo apt install gdal-bin libgdal-dev
sudo apt install python3.10-venv

# Instal as su to make them available system wide
sudo su - -c "R -e \"install.packages('devtools')\""
sudo su - -c "R -e \"install.packages('shiny')\""

sudo su - -c "R -e \"install.packages(c('clock', 'config', 'DBI', 'dplyr', 'DT', 'golem', 'hms', 'lubridate', 'plotly', 'raster', 'readr', 'reticulate', 'RSQLite', 'sf', 'shinydashboard', 'shinyFeedback'))\""
sudo su - -c "R -e \"install.packages(c('shinyWidgets', 'spsComps', 'suncalc', 'tidyr', 'uuid', 'waiter', 'widgetframe', 'shinyjs', 'sodium', 'shinymanager', 'keyring'))\""

# Rstuio server
sudo apt install gdebi-core
wget https://download2.rstudio.org/server/jammy/amd64/rstudio-server-2022.07.2-576-amd64.deb
sudo gdebi rstudio-server-2022.07.2-576-amd64.deb

# Shiny server
wget https://download3.rstudio.org/ubuntu-18.04/x86_64/shiny-server-1.5.19.995-amd64.deb
sudo gdebi shiny-server-1.5.19.995-amd64.deb

# edit server location and add the lines
# preserve_logs true;
#sanitize_errors false;
sudo nano /etc/shiny-server/shiny-server.conf

sudo systemctl restart shiny-server
sudo systemctl status shiny-server

# nginx redirection
sudo nano /etc/nginx/sites-enabled/default

sudo systemctl restart nginx
sudo systemctl status nginx


