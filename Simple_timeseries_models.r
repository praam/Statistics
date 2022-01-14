data1 <- read.csv("eComm_US.csv")
str(data1)

data1$DATE <- as.Date(data1$DATE)
data1$year = as.numeric(format(data1$DATE, "%Y"))
data1$month = as.numeric(format(data1$DATE, "%m"))
data1$day = as.numeric(format(data1$DATE, "%d"))

library(fpp2)

ts_sales_data <- ts(data1$ECOMNSA,start =c(data1$year[1],4),frequency = 4)
ts_sales_data
autoplot(ts_sales_data, xlab = "Time", ylab = "Sales in billions",main = "Time series of United States e-commerce retail sales (in $billions)")

library(TSstudio)
ts_info(ts_sales_data)

fit_decmult <- decompose(ts_sales_data,type = "multiplicative")

fit_decmult

l_tsales <- log(ts_sales_data)

autoplot(l_tsales,ylab="log(sales)",main="Log transformation of the time series object")

fit <- stl(l_tsales,s.window = "period")
fit

autoplot(fit,main="Season and trend decomposition using Loess")


#mean_model
ts_sales_data_mean<-meanf(ts_sales_data,h=3)
summary(ts_sales_data_mean)
autoplot(ts_sales_data_mean)

autoplot(ts_sales_data_mean)+autolayer(fitted(ts_sales_data_mean),series = "Fitted")


#Naive model
ts_sales_data_naive <- naive(ts_sales_data,h=3)
summary(ts_sales_data_naive)
autoplot(ts_sales_data_naive)

autoplot(ts_sales_data_naive)+autolayer(fitted(ts_sales_data_naive),series = "Fitted")

#Seasonal Naive model
ts_sales_data_seasonal_naive <- snaive(ts_sales_data,h=3)
summary(ts_sales_data_seasonal_naive)
autoplot(ts_sales_data_seasonal_naive)

autoplot(ts_sales_data_seasonal_naive)+autolayer(fitted(ts_sales_data_seasonal_naive),series = "Fitted")
