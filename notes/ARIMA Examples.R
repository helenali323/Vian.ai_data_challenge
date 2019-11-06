library(forecast)
library(tseries)

###################
## Chemical Data ###################################################################
###################
setwd("/users/ntstevens/Dropbox/Teaching/MSAN_604/2018/Lecture Material/")
par(mfrow=c(1,1))
chem <- read.csv("chemical.csv", header = F)
chem <- ts(chem$V1)
plot(chem, main = "Daily Chemical Concentrations", ylab = "Concentrations", xlab = "Days")

# Check whether ordinary differencing is necessary
acf(chem) #it seems necessary

# Apply ordinary differencing
dchem <- diff(chem)
par(mfrow = c(2,1))
ts.plot(dchem, main = "Differenced Daily Chemical Concentrations", ylab = "Concentrations", xlab = "Days")
acf(dchem, lag.max = 50)

# This seems stationary. Let's check with ADF test:
ndiffs(x = chem, test = "adf")
ndiffs(x = dchem, test = "adf")

adf.test(x = chem, alternative = "stationary")
adf.test(x = dchem, alternative = "stationary")

# So let's use ARIMA to model this. First we need to pick orders p and q.
# We do that with ACF and PACF plots:
par(mfrow=c(2,1))
acf(dchem, lag.max = 48)
pacf(dchem, lag.max = 48)

# p <= 2 and q <= 3 seems reasonable. Let's start there:
m <- arima(x = chem, order = c(2,1,3))
summary(m)

# What would auto.arima() have chosen?
auto.arima(chem)

# Let's visualize how well this model fits the data:
f <- chem - m$residuals #fitted values
par(mfrow=c(1,1))
plot(chem, type = "l", main = "Daily Chemical Concentrations", ylab = "Concentrations", xlab = "Days")
points(f, type = "l", col = "red")
legend("bottomright", legend = c("Observed", "Predicted"), lty = 1, col = c("black", "red"), cex = 0.5)

# Residual Diagnostics:
plot(m$residuals, main = "Residuals vs. Time", ylab = "Residuals")
abline(h = 0, col = "red")
acf(m$residuals, main = "ACF of Residuals")
qqnorm(m$residuals)
qqline(m$residuals, col = "red")
shapiro.test(m$residuals)

# Forecast
par(mfrow = c(1,1))
plot(forecast(object = m, h = 14, level = 0.95))

##########################
## Exercise: Crime Data ########################################################################
##########################
# Plot the data
par(mfrow=c(1,1))
crime <- read.csv(file = "UScrime.csv", header = F)
crime <- ts(data = crime$V1, start = 1933, end = 2015, frequency = 1)
plot(crime, main = "US Annual Crime Rates", xlab = "Year", ylab = "Crime Rate Per 100K Residents", xaxt = "n")
axis(side = 1, at = seq(1935, 2015, 5), labels = seq(1935, 2015, 5))

###########################
## Exercise: S&P500 Data ########################################################################
###########################
# Plot the data
par(mfrow=c(1,1))
sp500 <- read.csv(file = "sp500.csv", header = T)
plot(ts(data = sp500$AdjClose), main = "S&P500 Adjusted Closing Prices", xlab = "Date", ylab = "Price", xaxt = "n")
axis(side = 1, at = seq(1,length(sp500$Date), length.out = 10), labels = sp500$Date[seq(1,length(sp500$Date), length.out = 10)])

