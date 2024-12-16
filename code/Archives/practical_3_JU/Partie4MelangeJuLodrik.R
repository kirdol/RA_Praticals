#Libs
library(ggplot2)
library(dplyr)
library(MASS)
library(fitdistrplus)
library(evd)  # for extreme value distributions
library(ismev)
library(extRemes)
library(POT)
library(tidyverse)
# Load the data
data_temp <- read.csv("Data/Cleaned_Stations_Data.csv")

# Filter data for all cities and remove any rows with NA in TMAX
# Geneva
data_geneva <- data_temp %>%
  filter(!is.na(TMAX)) %>%
  filter(NAME == "Genève") %>%
  dplyr::select(DATE, TMAX) %>%
  mutate(DATE = as.Date(DATE))

# Santïs
data_santis <- data_temp %>%
  filter(!is.na(TMAX)) %>%
  filter(NAME == "Saentis") %>%
  dplyr::select(DATE, TMAX) %>%
  mutate(DATE = as.Date(DATE))

# Lugano
data_lugano <- data_temp %>%
  filter(!is.na(TMAX)) %>%
  filter(NAME == "Lugano") %>%
  dplyr::select(DATE, TMAX) %>%
  mutate(DATE = as.Date(DATE))

# Calculate annual maximum temperatures
# Geneva
annual_maxima_geneva <- data_geneva %>%
  mutate(Year = year(DATE)) %>%
  group_by(Year) %>%
  summarize(MaxTemp = max(TMAX), .groups = 'drop')

# Santïs
annual_maxima_santis <- data_santis %>%
  mutate(Year = year(DATE)) %>%
  group_by(Year) %>%
  summarize(MaxTemp = max(TMAX), .groups = 'drop')

# Lugano
annual_maxima_Lugano <- data_lugano %>%
  mutate(Year = year(DATE)) %>%
  group_by(Year) %>%
  summarize(MaxTemp = max(TMAX), .groups = 'drop')

# Fit a GEV distribution to the annual maxima
# Genève
gev_fit_geneva <- gev.fit(annual_maxima_geneva$MaxTemp)

# Santïs
gev_fit_santis <- gev.fit(annual_maxima_santis$MaxTemp)

# Lugano
gev_fit_Lugano <- gev.fit(annual_maxima_Lugano$MaxTemp)

#Diagnostics

gev.diag(gev_fit_geneva)

gev_diag(gev_fit_santis)

gev.diag(gev_fit_Lugano)

print(gev_fit_geneva)


# Function to compute log-likelihood, AIC, and BIC
compute_aic_bic_manual <- function(fit, data) {
  log_likelihood <- sum(dgev(data, 
                             loc = fit$mle[1], 
                             scale = fit$mle[2], 
                             shape = fit$mle[3], 
                             log = TRUE))
  k <- length(fit$mle)  # Number of parameters
  n <- length(data)     # Number of observations
  
  aic <- -2 * log_likelihood + 2 * k
  bic <- -2 * log_likelihood + k * log(n)
  
  return(list(LogLikelihood = log_likelihood, AIC = aic, BIC = bic))
}

# Calculate for each dataset
aic_bic_geneva <- compute_aic_bic_manual(gev_fit_geneva, annual_maxima_geneva$MaxTemp)
aic_bic_santis <- compute_aic_bic_manual(gev_fit_santis, annual_maxima_santis$MaxTemp)
aic_bic_lugano <- compute_aic_bic_manual(gev_fit_Lugano, annual_maxima_Lugano$MaxTemp)

# Display results
print("Geneva:")
print(aic_bic_geneva)

print("Santïs:")
print(aic_bic_santis)

print("Lugano:")
print(aic_bic_lugano)



# Peaks-over-Threshold Approach (GPD Distribution)


# Filter the data for the summer months (June to September)
# Geneva
geneva_summer <- subset(data_geneva, format(DATE, "%m") %in% c("06", "07", "08", "09"))

# Santis
santis_summer <- subset(data_santis, format(DATE, "%m") %in% c("06", "07", "08", "09"))

# Lugano
lugano_summer <- subset(data_lugano, format(DATE, "%m") %in% c("06", "07", "08", "09"))


# Define a threshold at the 95th percentile
# Geneva
threshold_geneva <- quantile(geneva_summer$TMAX, 0.95)

# Santïs
threshold_santis <- quantile(santis_summer$TMAX, 0.95)

# Lugano
threshold_lugano <- quantile(lugano_summer$TMAX, 0.95)


# Number of exceedances over the threshold
# Geneva
num_exceedances_geneva <- sum(geneva_summer$TMAX > threshold_geneva)

# Santïs
num_exceedances_santis <- sum(santis_summer$TMAX > threshold_santis)

# Lugano
num_exceedances_Lugano <- sum(lugano_summer$TMAX > threshold_lugano)

# Decluster the data using the chosen threshold for each station
# Genava
geneva_declustered <- extRemes::decluster(geneva_summer$TMAX, threshold = threshold_geneva, run.length = 1)

# Santïs
santis_declustered <- extRemes::decluster(santis_summer$TMAX, threshold = threshold_santis, run.length = 1)

# Lugano
lugano_declustered <- extRemes::decluster(lugano_summer$TMAX, threshold = threshold_lugano, run.length = 1)


# Add the declustered data to the corresponding datasets
# Geneva
geneva_summer$declustered <- ifelse(geneva_summer$TMAX >= threshold_geneva, geneva_declustered, NA)

# Santïs
santis_summer$declustered <- ifelse(santis_summer$TMAX >= threshold_santis, santis_declustered, NA)

# Lugano
lugano_summer$declustered <- ifelse(lugano_summer$TMAX >= threshold_lugano, lugano_declustered, NA)


# Extract declustered extreme values
# Geneva
extreme_values_geneva <- geneva_summer[["declustered"]][!is.na(geneva_summer[["declustered"]])]

# Santïs
extreme_values_santis <- santis_summer[["declustered"]][!is.na(santis_summer[["declustered"]])]

# Lugano
extreme_values_lugano <- lugano_summer[["declustered"]][!is.na(lugano_summer[["declustered"]])]

# Fit a GPD to the exceedances
# Genève
gpd_fit_geneva <- fevd(extreme_values_geneva, threshold = threshold_geneva, type = "GP")

# Santïs
gpd_fit_santis <- fevd(extreme_values_santis, threshold = threshold_santis, type = "GP")

# Lugano
gpd_fit_lugano <- fevd(extreme_values_lugano, threshold = threshold_lugano, type = "GP")

# Diagnostic plots for the GPD fit
# Genève
par(mfrow = c(2, 2))
plot(gpd_fit_geneva)

# Santïs
par(mfrow = c(2, 2))
plot(gpd_fit_santis)

# Lugano
par(mfrow = c(2, 2))
plot(gpd_fit_lugano)


# Calculate return levels for 10, 50, and 100-year return periods
# Geneva
gpd_return_levels_geneva <- return.level(gpd_fit_geneva, return.period = c(10, 50, 100))

# Santïs
gpd_return_levels_santis <- return.level(gpd_fit_santis, return.period = c(10, 50, 100))

# Lugano
gpd_return_levels_Lugano <- return.level(gpd_fit_lugano, return.period = c(10, 50, 100))

# Function to compute AIC and BIC for fevd objects
compute_aic_bic_gpd <- function(fit, data) {
  log_likelihood <- fit$results$value  # Log-likelihood from the fit object
  k <- length(fit$results$par)         # Number of parameters (scale, shape)
  n <- length(data)                    # Number of data points (declustered values)
  
  aic <- -2 * log_likelihood + 2 * k
  bic <- -2 * log_likelihood + k * log(n)
  
  return(list(AIC = aic, BIC = bic))
}

# Compute AIC and BIC for each fit
aic_bic_geneva <- compute_aic_bic_gpd(gpd_fit_geneva, extreme_values_geneva)
aic_bic_santis <- compute_aic_bic_gpd(gpd_fit_santis, extreme_values_santis)
aic_bic_lugano <- compute_aic_bic_gpd(gpd_fit_lugano, extreme_values_lugano)

# Display results
print("Geneva AIC and BIC:")
print(aic_bic_geneva)

print("Santis AIC and BIC:")
print(aic_bic_santis)

print("Lugano AIC and BIC:")
print(aic_bic_lugano)




