data1 <- read.csv("eComm_US.csv")

data1$DATE <- as.Date(data1$DATE)
data1$year = as.numeric(format(data1$DATE, "%Y"))
data1$month = as.numeric(format(data1$DATE, "%m"))
data1$day = as.numeric(format(data1$DATE, "%d"))


library(fpp2)

tsales <- ts(data1$ECOMNSA,start = c(data1$year[1],4),frequency = 4)
autoplot(tsales)

library(TSstudio)
ts_info(tsales)


components.fit <- decompose(tsales)
plot(components.fit)




install.packages("fUnitRoots")

library("fUnitRoots")
urkpssTest(tsales, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(tsales, differences=1)
plot(tsstationary)


acf(tsales,lag.max=34)


timeseriesseasonallyadjusted <- tsales- components.fit$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)

acf(tsstationary, lag.max=34)
pacf(tsstationary, lag.max=34)


fitARIMA <- arima(tsales, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")
library(lmtest)
coeftest(fitARIMA)

ggtsdisplay(tsales)

d = ndiffs(tsales)


