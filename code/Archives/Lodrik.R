# Plot the negative log returns series
plot(neg_log_returns_bitcoin,
     type="l",
     col="blue", 
     main="Negative Log Returns of Bitcoin Prices", 
     xlab="Time",
     ylab="Negative Log Returns")

# plot the Bitcoin prices
plot(bitcoin_prices, type="l", col="red", 
     main="Bitcoin Prices", xlab="Time", ylab="Price")