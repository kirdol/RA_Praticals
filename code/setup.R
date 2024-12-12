# Set the CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# load the required packages and install them if they are not.
packages <- c(
  "here",
  "tseries",
  "nortest",
  "stats",
  "MASS",
  "fpp3",
  "urca",
  "ggplot2",
  "patchwork",
  "forecast",
  "fGarch",
  "fitdistrplus",
  "lmtest",
  "lubridate",
  "evd",
  "evir",
  "ismev",
  "tidyverse",
  "extRemes",
  "zoo",
  "forecast"
)

# Function that install the packages if not already installed on your computer
for (pkg in packages) {
  if (!pkg %in% installed.packages()) {
    install.packages(pkg)}}

# load the packages
for (pkg in packages) {
  library(pkg, character.only = TRUE)}