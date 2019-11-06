library(forecast)
library(tseries)

#######################
## Airpassenger Data ########################################################################
#######################
# Use the AirPassengers data. Recall we log-transform it to remove heteroscedasticity.
par(mfrow=c(1,1))
plot(AirPassengers)
par(mfrow=c(2,1))
plot(log(AirPassengers))
acf(log(AirPassengers), lag.max = 48)

# The raw time series is clearly not stationary. Try differencing once:
AP1 <- diff(log(AirPassengers))
plot(AP1, ylab = "AP1")
acf(AP1, lag.max = 144)
adf.test(AP1)

# There still seems to be monthly seasonality (period = 12). Let's try differencing for that.
AP1.12 <- diff(AP1, lag = 12)
plot(AP1.12)
acf(AP1.12, lag.max = 144)

# Let's use hypothesis tests to decide how much differencing might have been necessary
ndiffs(log(AirPassengers))
nsdiffs(log(AirPassengers), m = 12)


# This looks good, so choose d=1, D=1, s=12. Let's check ACF and PACF to choose p, q, P, Q.
acf(AP1.12, lag.max = 48)
pacf(AP1.12, lag.max = 48)

# Maybe p <= 3, q = 1, and P = Q = 1
m <- arima(log(AirPassengers), order = c(3,1,1), seasonal = list(order = c(1,1,1), period = 12), method = "CSS-ML")
summary(m)

# Plot the times series and fitted values
par(mfrow=c(1,1))
plot(log(AirPassengers))
fit <- log(AirPassengers) - m$residuals
lines(fit, col = "red")
legend("bottomright", legend = c("Observed", "Predicted"), lty = 1, col = c("black", "red"), cex = 0.5)

# Recreate plots on the raw scale
plot(AirPassengers)
lines(exp(fit), col = "red")
legend("bottomright", legend = c("Observed", "Predicted"), lty = 1, col = c("black", "red"), cex = 0.5)

# Let's see which model auto.arima chooses as being optimal
auto.arima(log(AirPassengers), allowdrift = FALSE)

# We should check model diagnostics as we normally would with an ARMA model. Specifically, checking the heteroscedasticity 
# assumption can lead to an optimal choice of variance-stabilizing transformation
plot(m$residuals, main = "Residuals vs. Time", ylab = "Residuals")
abline(h = 0, col = "red")
acf(m$residuals, main = "ACF of Residuals")
qqnorm(m$residuals)
qqline(m$residuals, col = "red")
shapiro.test(m$residuals)

# Forecast
plot(forecast(object = m, h = 60, level = 0.95))

##############
## Wine Data ########################################################################
##############
setwd("/users/ntstevens/Dropbox/Teaching/MSAN_604/2018/Lecture Material/")
wine <- read.csv("wine.csv", header = TRUE)
wine <- wine$Consumption
wine <- ts(data = wine, start = c(2001,1), frequency = 12)
plot(wine, main = "Monthly Wine Consumption", ylab = "Consumption", xlab = "Month")

# Check whether ordinary and/or seasonal differencing seems necessary
par(mfrow=c(1,1))
acf(wine, lag.max = 48)

# Both forms of differencing seems necessary. Let's do ordinary first:
dwine <- diff(wine)
par(mfrow=c(2,1))
plot(dwine, main = "Trend Adjusted Monthly Wine Consumption", ylab = "Consumption", xlab = "Month")
acf(dwine, lag.max = 48)

# Still need seasonal differencing:
dwine.12 <- diff(dwine, lag = 12)
par(mfrow=c(2,1))
plot(dwine.12, main = "Trend and Seasonally Adjusted Monthly Wine Consumption", ylab = "Consumption", xlab = "Month")
acf(dwine.12, lag.max = 48)

# This seems fine now. Since we seasonally differenced, we are fitting a SARIMA 
# model and need to choose p, q, P, Q. Let's look at the ACF/PACF plots for this
par(mfrow=c(2,1))
acf(dwine.12, lag.max = 48)
pacf(dwine.12, lag.max = 48)

# p = 4, q <= 1, P = Q = 1?
m <- arima(x = wine, order = c(4,1,1), seasonal = list(order = c(1,1,1), period = 12))
m

# What would auto.arima() have chosen?
auto.arima(wine, allowdrift = FALSE)

# Let's visualize how well this model fits the data:
f <- wine - m$residuals #fitted values
par(mfrow=c(1,1))
plot(wine, type = "l", main = "Monthly Wine Consumption", ylab = "Consumption", xlab = "Month")
points(f, type = "l", col = "red")
legend("bottomright", legend = c("Observed", "Predicted"), lty = 1, col = c("black", "red"), cex = 0.5)

# Residual Diagnostics:
par(mfrow=c(1,1))
plot(m$residuals, main = "Residuals vs. Time", ylab = "Residuals")
abline(h = 0, col = "red")
acf(m$residuals, main = "ACF of Residuals")
qqnorm(m$residuals)
qqline(m$residuals, col = "red")
shapiro.test(m$residuals)

# Forecast
plot(forecast(object = m, h = 60, level = 0.95))

###########################
## Exercise: USAccDeaths ####################################
###########################
par(mfrow=c(1,1))
plot(USAccDeaths, main = "Monthly Accidental Deaths in the US", xlab = "Year", ylab = "Number of Deaths")

#######################
## Exercise: ldeaths ########################################
#######################
par(mfrow=c(1,1))
plot(ldeaths, main = "Monthly Deaths from Lung Disease in the UK", xlab = "Year", ylab = "Number of Deaths")


