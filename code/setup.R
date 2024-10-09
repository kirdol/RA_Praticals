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
  "patchwork"
)

# Function that install the packages if not already installed on your computer
for (pkg in packages) {
  if (!pkg %in% installed.packages()) {
    install.packages(pkg)}}

# load the packages
for (pkg in packages) {
  library(pkg, character.only = TRUE)}