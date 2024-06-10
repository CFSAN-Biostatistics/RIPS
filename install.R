#!/usr/bin/Rscript

#Modified from genomeSidekick:https://github.com/dchapski/genomeSidekick
# This script should get you the dependencies required for RIPS
# Might want to run by hand so you do not overwrite custom installations
# More info:
# https://statsandr.com/blog/an-efficient-way-to-install-and-load-r-packages/

# Define a function to negate the %in% function in R
`%notin%` <- Negate(`%in%`)

# List all dependencies
dependencies <- c("rvest", "httr", "RCurl", "dplyr", "shiny", "data.table", "DT", "purrr", "xml2", "ggplot2", "lubridate", "plotly", "stringr", "tidyr", "readr", "shinydashboard", "shinyWidgets", "rintrojs"
)

# Check which ones are not met by using %notin%
not.met <- dependencies %notin% rownames(installed.packages())

# Install unmet dependencies
if(any(not.met)) {
  install.packages(dependencies[not.met])
}
