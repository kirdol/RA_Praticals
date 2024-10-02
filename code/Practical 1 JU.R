install.packages("fpp3")
install.packages("urca")
install.packages("patchwork")
install.packages("nortest")
install.packages("MASS")
install.packages("tseries")
install.packages("forecast")
install.packages("fGarch")
################################################Part 1
#Seeing if the time series is stationary :
library(fpp3)
library(patchwork)

data <- read.csv("Crypto_data.csv")

#Adding a "time" column
data <- data |>
  mutate(Time = row_number())

#Transforming into a tsibble
data <- as_tsibble(data, index = Time)

#Testing for stationarity
data |> features(Bitcoin, unitroot_kpss)
#p-value of 0.01 indicates strong evidence to reject null hypothesis 
#of stationary

#1b)

neg_log_return <- function(prices) { # Calculate log returns: log(Xt/Xt-1)
  
  log_returns <- diff(log(prices))
  
  # Return the negative of the log returns
  return(-log_returns)
}

neglogret <- neg_log_return(data$Bitcoin)

row_count <- seq_along(neglogret)

nlrdf <- data.frame(Time = row_count, nlr = neglogret)

ggplot(nlrdf, aes(x = Time, y = nlr)) +
  geom_line()   # Line plot
 
#Stationarity

nlrdf <- as_tsibble(nlrdf, index = Time)

nlrdf |> features(nlr, unitroot_kpss)
#Seems stationary, check other ways to test like autocorr plots
nlrdf |> ACF(nlr) |> autoplot()
#Pretty much stationary except for certain early lags


#Plotting on same graph


# Install patchwork if not already installed

# Plot 1: Bitcoin Prices over Time
p1 <- ggplot(data, aes(x = Time, y = Bitcoin)) +
  geom_line(color = "blue") +
  labs(title = "Bitcoin Prices over Time", x = "Time", y = "Bitcoin Prices") +
  theme_minimal()

# Plot 2: Negative Log Returns (nlr) over Time
p2 <- ggplot(nlrdf, aes(x = Time, y = nlr)) +
  geom_line(color = "red") +
  labs(title = "Negative Log Returns of Bitcoin", x = "Time", y = "Negative Log Return") +
  theme_minimal()

# Combine the two plots using patchwork
p1 + p2

#Tiny variations on the nlr graph compared to btc over time

#1c)
#Histogram

ggplot(nlrdf, aes(x = nlr)) +
  geom_histogram(binwidth = 0.01, color = "black", fill = "lightblue") +
  labs(title = "Histogram of Negative Log Returns", x = "Negative Log Returns", y = "Frequency") +
  theme_minimal()


#QQ-plot
ggplot(nlrdf, aes(sample = nlr)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ-Plot of Negative Log Returns") +
  theme_minimal()

#Anderson-Darling test
library(nortest)

# Perform the Anderson-Darling test
ad_test <- ad.test(nlrdf$nlr)
print(ad_test)

#With all of these we reject normality hypothesis

#1d)
?fitdistr()
# Load required libraries
library(MASS)

# Step 1: Fit a t-distribution to the negative log returns
# For a t-distribution, we need to specify the degrees of freedom (df), location (mean), and scale (sd)
# In this case, we estimate these using fitdistr()

fit_t <- fitdistr(nlrdf$nlr, "t", start = list(m = 0, s = 1, df = 5))  # Starting values

#################################################################ERROR USING THIS, ASK TEACHER
# Print the estimated parameters of the t-distribution
print(fit_t)

# Step 2: Generate a QQ-plot to assess the fit
# Extract the estimated parameters
df_est <- fit_t$estimate["df"]
mean_est <- fit_t$estimate["m"]
sd_est <- fit_t$estimate["s"]

# Generate a sample of t-distributed values based on the estimated parameters
t_sample <- rt(length(nlrdf$nlr), df = df_est) * sd_est + mean_est

# Create a QQ-plot comparing the negative log returns to the fitted t-distribution
ggplot(nlrdf, aes(sample = nlr)) +
  stat_qq(distribution = qt, dparams = list(df = df_est)) +
  stat_qq_line(distribution = qt, dparams = list(df = df_est)) +
  labs(title = "QQ-Plot of Negative Log Returns vs. Fitted t-Distribution") +
  theme_minimal()

#REST PROBABLY WORKS ONCE FIRST LINE WORKS


#1e) depends on d) answer


##########################################################################Part 2

#2a)
nlrdf |> ACF(nlr) |> autoplot()
data |> ACF(Bitcoin) |> autoplot()

#the negative log returns by far

#2b)

# Load necessary packages
library(tseries)

# Perform Ljung-Box test on the raw series
ljung_box_raw <- Box.test(data$Bitcoin, lag = 10, type = "Ljung-Box")
print(ljung_box_raw)

#p-value < 2.2e-16

#Now on nlr
ljung_box_nlr <- Box.test(nlrdf$nlr, lag = 10, type = "Ljung-Box")
print(ljung_box_nlr)

#p-value = 0.002845

#Conclusion : very low p-values for both, leading us to reject null hypothesis 
#both times and indicating the presence of significant autocorrelation in both 
#series.  

#2c)
nlrdf |> PACF(nlr)|> autoplot()
nlrdf |> ACF(nlr) |> autoplot()

#ARIMA fit
arima <- nlrdf|> model(ARIMA(nlr))
report(arima)
#Suggested model : ARIMA(2,0,2)

#Like teacher wants
library(forecast)
autoarima <- auto.arima(nlrdf$nlr)

# Print the selected model
print(autoarima)
#Once again model ARIMA(2,0,2) chosen

# Plot diagnostic checks for the model
dev.new() #Use this to make plot panel bigger

tsdiag(autoarima)

#Assessing residuals
checkresiduals(autoarima)
#Residuals looking good,although probable violation of homoscedasticity (check
#ACF)

#2d)
library(fGarch)
#Fit a GARCH(1,1) model with a normal distribution
garch_normal <- garchFit(~ garch(1, 1), data = nlrdf$nlr, cond.dist = "norm", trace = FALSE)
summary(garch_normal)

#Fit a GARCH(1,1) model with a standardized t-distribution
garch_t <- garchFit(~ garch(1, 1), data = nlrdf$nlr, cond.dist = "std", trace = FALSE)
summary(garch_t)

#Diagnostic plots for residuals
# Residuals of the GARCH(1,1) model with normal distribution
par(mfrow = c(2, 2))  
plot(garch_normal, which = 13)  # Standardized Residuals
plot(garch_normal, which = 9)   # ACF of the Standardized Residuals
plot(garch_normal, which = 10)  # ACF of the Squared Standardized Residuals
plot(garch_normal, which = 11)  # QQ-plot of the Standardized Residuals

#Diagnostic plots for residuals of the t-distribution model
par(mfrow = c(2, 2))  
plot(garch_t, which = 13)  # Standardized Residuals
plot(garch_t, which = 9)   # ACF of the Standardized Residuals
plot(garch_t, which = 10)  # ACF of the Squared Standardized Residuals
plot(garch_t, which = 11)  # QQ-plot of the Standardized Residuals

#2e)

#Fitting GARCH on residuals of ARIMA fit 

#Extract residuals from the ARIMA model
arima_residuals <- residuals(autoarima)

#Fit a GARCH(1,1) model to the residuals of the ARIMA model
garch_fit_on_residuals <- garchFit(~ garch(1, 1), data = arima_residuals, cond.dist = "norm", trace = FALSE)
summary(garch_fit_on_residuals)

# Step 4: Plot diagnostic checks for GARCH model residuals
par(mfrow = c(2, 2))  
plot(garch_fit_on_residuals, which = 13)  # Standardized Residuals
plot(garch_fit_on_residuals, which = 9)   # ACF of the Standardized Residuals
plot(garch_fit_on_residuals, which = 10)  # ACF of Squared Standardized Residuals
plot(garch_fit_on_residuals, which = 11)  # QQ-plot of the Standardized Residuals

#2f)
#Comparison : Compare AIC and other criterion. 
#Garch with t distrib seems to be the best.
#Homoscedasticity hypothesis only violated for the ARIMA(2,0,2) model.


#####################################################################################Part 3
