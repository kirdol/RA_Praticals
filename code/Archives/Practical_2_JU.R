#Part 1
library(ggplot2)
library(dplyr)
library(MASS)
library(fitdistrplus)
library(evd)  # for extreme value distributions
library(ismev)
library(extRemes)
library(POT)
##################################a)
datageneva <- read.csv("data/Geneva_temperature.csv")
datalausanne <- read.csv("data/Precipitation_lausanne_full.csv")

datalausanne$Date <- as.Date(datalausanne$Date, format = "%m/%d/%Y")

ggplot(datalausanne, aes(x = Precipitation)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Daily Precipitation in Lausanne", x = "Daily Precipitation (mm)", y = "Frequency")+
  theme_minimal()

#Not sure which distribution would best fit the data : Gamma, Weiubl, Fréchet ...

###############################b)
# Extract year from the Date column
datalausanne$Year <- format(datalausanne$Date, "%Y")

# Calculate the yearly maximum precipitation
yearly_max_precip <- aggregate(Precipitation ~ Year, data = datalausanne, max)

# Convert Year to numeric for plotting purposes
yearly_max_precip$Year <- as.numeric(yearly_max_precip$Year)

# Plot histogram of yearly maximum values
ggplot(yearly_max_precip, aes(x = Precipitation)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Yearly Maximum Precipitation in Lausanne",
       x = "Yearly Maximum Precipitation (mm)",
       y = "Frequency") +
  theme_minimal()

# Fit a GEV distribution (block maxima approach)
gev_fit <- gev.fit(yearly_max_precip$Precipitation)

# Plotting diagnostic for GEV fit (optional)
gev.diag(gev_fit)

# Print GEV fit results
print(gev_fit)

#Looks pretty good ?

#######################################c)

# Fit a linear model to yearly maximum values
lm_model <- lm(Precipitation ~ Year, data = yearly_max_precip)

# Summarize the linear model
summary(lm_model)

# Create a data frame for the next 10 years
future_years <- data.frame(Year = seq(max(yearly_max_precip$Year) + 1, by = 1, length.out = 10))

# Predict precipitation for the next 10 years with confidence intervals
predictions <- predict(lm_model, newdata = future_years, interval = "confidence")

# Combine predictions with future years
predicted_data <- data.frame(Year = future_years$Year, Predicted = predictions[, "fit"],
                             Lower_CI = predictions[, "lwr"], Upper_CI = predictions[, "upr"])

# Plot the observed yearly maximums and predictions with confidence intervals
ggplot() +
  geom_point(data = yearly_max_precip, aes(x = Year, y = Precipitation), color = "blue") +
  geom_line(data = yearly_max_precip, aes(x = Year, y = Precipitation), color = "blue") +
  geom_line(data = predicted_data, aes(x = Year, y = Predicted), color = "red", linetype = "dashed") +
  geom_ribbon(data = predicted_data, aes(x = Year, ymin = Lower_CI, ymax = Upper_CI), alpha = 0.2, fill = "red") +
  labs(title = "Yearly Maximum Precipitation with Predictions and Confidence Intervals",
       x = "Year", y = "Maximum Precipitation (mm)") +
  theme_minimal()

#This is probably quite a bad approach, as the forecast does not take into account any of the intricacies of 
#the rainfall pattern, providing a bad forecast.

##############################d)

# Fit a GEV model with constant parameters using fevd function
gev_model_constant <- fevd(yearly_max_precip$Precipitation, type = "GEV")

# Fit a GEV model with time-varying location parameter
# To do this, we include 'Year' as a covariate for the location parameter
gev_model_time_varying <- fevd(yearly_max_precip$Precipitation, data = yearly_max_precip, type = "GEV", 
                               location.fun = ~ Year)

# Extract log-likelihoods from the models
logLik_constant <- -gev_model_constant$results$value
logLik_time_varying <- -gev_model_time_varying$results$value

# Define the number of parameters and observations
n_constant <- length(gev_model_constant$results$par)
n_time_varying <- length(gev_model_time_varying$results$par)
n_obs <- nrow(yearly_max_precip)

# Calculate AIC and BIC manually
aic_constant <- -2 * logLik_constant + 2 * n_constant
aic_time_varying <- -2 * logLik_time_varying + 2 * n_time_varying
bic_constant <- -2 * logLik_constant + log(n_obs) * n_constant
bic_time_varying <- -2 * logLik_time_varying + log(n_obs) * n_time_varying

# Print AIC and BIC values for comparison
cat("AIC for Constant GEV Model:", aic_constant, "\n")
cat("AIC for Time-Varying Location GEV Model:", aic_time_varying, "\n")
cat("BIC for Constant GEV Model:", bic_constant, "\n")
cat("BIC for Time-Varying Location GEV Model:", bic_time_varying, "\n")

#Constant GEV model seems to be the best.

# Summaries for interpretation
summary(gev_model_constant)
summary(gev_model_time_varying)

#e)

# Fit the GEV model with constant parameters to get an object compatible with gev.diag
gev_model_constant_ismev <- gev.fit(yearly_max_precip$Precipitation)

# Diagnostic plots for the constant GEV model
gev.diag(gev_model_constant_ismev)

#Seems to be a good fit

###################################################f)
# Calculate the 10-year return level
# The return level for a period T is calculated as: 
# RL_T = μ + (σ / ξ) * [(−log(1 − 1/T))^(−ξ) − 1]
mu <- gev_model_constant_ismev$mle[1]   # Location parameter
sigma <- gev_model_constant_ismev$mle[2]  # Scale parameter
xi <- gev_model_constant_ismev$mle[3]    # Shape parameter

# Return period T = 10 years
T <- 10
return_level_10 <- mu + (sigma / xi) * ((-log(1 - 1/T))^(-xi) - 1)

# Plot the observed yearly maxima and the 10-year return level
ggplot(yearly_max_precip, aes(x = Year, y = Precipitation)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_hline(yintercept = return_level_10, linetype = "dashed", color = "red") +
  labs(title = "10-Year Return Level Prediction with Yearly Maximum Precipitation",
       x = "Year", y = "Precipitation (mm)") +
  annotate("text", x = min(yearly_max_precip$Year), y = return_level_10, 
           label = paste("10-Year Return Level:", round(return_level_10, 2), "mm"),
           vjust = -1, hjust = -0.1, color = "red") +
  theme_minimal() +
  theme(legend.position = "top")

# Print the return level for reference
cat("The predicted 10-year return level is:", return_level_10, "mm\n")

###############################################g)

# Define return periods
return_periods <- c(10, 20, 50, 85)

# Calculate return levels for each return period
return_levels <- sapply(return_periods, function(T) {
  mu + (sigma / xi) * ((-log(1 - 1/T))^(-xi) - 1)
})

# Create a data frame with return levels for easier plotting and comparison
return_levels_df <- data.frame(Return_Period = return_periods, Return_Level = return_levels)

# Count the number of yearly maxima exceeding each return level
exceed_counts <- sapply(return_levels, function(level) {
  sum(yearly_max_precip$Precipitation > level)
})

# Combine results for easy display
results <- data.frame(Return_Period = return_periods, 
                      Return_Level = return_levels,
                      Exceed_Count = exceed_counts)

# Print the results
print(results)

# Plot the historical data with return levels for visual comparison
ggplot(yearly_max_precip, aes(x = Year, y = Precipitation)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_hline(data = return_levels_df, aes(yintercept = Return_Level, color = as.factor(Return_Period)),
             linetype = "dashed") +
  labs(title = "Yearly Maximum Precipitation with Return Levels",
       x = "Year", y = "Precipitation (mm)", color = "Return Period (Years)") +
  theme_minimal()

# Comment on results
cat("Return level results and historical exceedances:\n")
for (i in 1:nrow(results)) {
  cat(paste0("Return Period: ", results$Return_Period[i], "-Year\n",
             "Return Level: ", round(results$Return_Level[i], 2), " mm\n",
             "Historical Exceedances: ", results$Exceed_Count[i], "\n\n"))
}

############################### h)

# Define the threshold for precipitation
threshold <- 100

# Check if the threshold is within the domain of the CDF
if (1 + xi * (threshold - mu) / sigma > 0) {
  # Compute the CDF for the given threshold
  F_x <- exp(-(1 + xi * (threshold - mu) / sigma)^(-1 / xi))
  
  # Calculate the return period for the threshold
  return_period <- 1 / (1 - F_x)
  
  # Print the return period result
  cat("The return period for 100 mm of precipitation is approximately:", round(return_period, 2), "years.\n")
} else {
  cat("The threshold of 100 mm is outside the range of the fitted GEV model.\n")
}


##################################i)

# Define the threshold for precipitation
threshold <- 150

# Check if the threshold is within the domain of the CDF
if (1 + xi * (threshold - mu) / sigma > 0) {
  # Compute the CDF for the threshold
  F_x <- exp(-(1 + xi * (threshold - mu) / sigma)^(-1 / xi))
  
  # Compute the probability of exceeding 150 mm on a given day
  p_daily_exceedance <- 1 - F_x
  
  # Compute the probability of at least one exceedance in the next year (365 days)
  p_yearly_exceedance <- 1 - (1 - p_daily_exceedance)^365
  
  # Print the result
  cat("The probability of at least one day with precipitation exceeding 150 mm in the next year is approximately:",
      round(p_yearly_exceedance * 100, 2), "%\n")
} else {
  cat("The threshold of 150 mm is outside the range of the fitted GEV model.\n")
}


################################################################################################## Part 2

#a)

# Plot the time series of daily precipitation
plot_2a <- ggplot(datalausanne, aes(x = Date, y = Precipitation)) +
  geom_line(color = "blue") +
  labs(title = "Daily Precipitation in Lausanne (1930-2014)",
       x = "Date", y = "Precipitation (mm)") +
  theme_minimal()

plot_2a

################################################b)
# Mean Residual Life Plot
mrplot <- mrlplot(datalausanne$Precipitation, main = "Mean Residual Life Plot for Daily Precipitation")

# Choose a threshold based on the MRL plot (for example, say 20 mm as a preliminary choice)
threshold <- 50  # Adjust this based on MRL plot interpretation

# Highlight data points exceeding the chosen threshold
datalausanne$Exceeds_Threshold <- datalausanne$Precipitation > threshold

# Plot time series with highlighted exceedances
ggplot(datalausanne, aes(x = Date, y = Precipitation)) +
  geom_line(color = "blue") +
  geom_point(data = subset(datalausanne, Exceeds_Threshold), 
             aes(x = Date, y = Precipitation), color = "red") +
  labs(title = "Daily Precipitation in Lausanne with Exceedances Highlighted",
       x = "Date", y = "Precipitation (mm)") +
  theme_minimal()

#Not too sure about where the threshold should be, around 50 / 60 ?

###########################################c)

gpd_fit <- fitgpd(datalausanne$Precipitation, threshold)

# Print the GPD fit results to view parameter estimates
print(gpd_fit)

# Diagnostic plots to evaluate the GPD fit
par(mfrow = c(2, 2))  # Set up a 2x2 plot layout
plot(gpd_fit)

#Seems reasonable

#################################################d)

# Define the return periods
return_periods <- c(10, 20, 50, 85)

# Extract parameters from the fitted model
sigma <- gpd_fit$param["scale"]
xi <- gpd_fit$param["shape"]

# Calculate the rate of exceedance
n_total <- nrow(datalausanne)
n_exceedances <- sum(datalausanne$Precipitation > threshold)
lambda <- n_exceedances / n_total  # Proportion of exceedances

# Calculate return levels for each return period
return_levels <- sapply(return_periods, function(T) {
  threshold + (sigma / xi) * ((T * lambda)^xi - 1)
})

# Create a data frame for easy viewing
return_levels_df <- data.frame(Return_Period = return_periods, Return_Level = return_levels)

# Print the return levels
print(return_levels_df)

# Explanation of results:
# The resulting return levels indicate the precipitation amount expected to be exceeded, on average,
# once every 10, 20, 50, and 85 years, according to the GPD model.

###################################################e)
precip_level <- 100

# Compute the return period for the specified precipitation level
if (1 + xi * (precip_level - threshold) / sigma > 0) {
  return_period <- (1 / lambda) * (1 + (xi * (precip_level - threshold) / sigma))^(1 / xi)
  return_period_years <- return_period / 365
  cat("The return period for a precipitation level of 100 mm is approximately:", round(return_period_years, 2), "years.\n")
} else {
  cat("The level of 100 mm is outside the range of the fitted GPD model.\n")
}
#Dont forget to turn into days or result totally outside of scale.

#####################################################f)
precip_level <- 150

# Check that the threshold condition holds
if (1 + xi * (precip_level - threshold) / sigma > 0) {
  # Compute the exceedance probability for 150 mm
  daily_exceedance_prob <- 1 - (1 + xi * (precip_level - threshold) / sigma)^(-1 / xi)
  
  # Compute the probability of at least one exceedance in the next year (365 days)
  yearly_exceedance_prob <- 1 - (1 - daily_exceedance_prob)^365
  
  # Print the result
  cat("The probability of at least one day with precipitation exceeding 150 mm in the next year is approximately:",
      round(yearly_exceedance_prob * 100, 2), "%\n")
} else {
  cat("The level of 150 mm is outside the range of the fitted GPD model.\n")
}
