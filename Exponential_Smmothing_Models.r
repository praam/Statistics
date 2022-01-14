data2<-read.csv("eComm_US.csv")

data2$DATE <- as.Date(data2$DATE)
data2$year = as.numeric(format(data2$DATE, "%Y"))
data2$month = as.numeric(format(data2$DATE, "%m"))
data2$day = as.numeric(format(data2$DATE, "%d"))

library(fpp2)

time_sales <- ts(data2$ECOMNSA,start = c(data2$year[1],4),frequency = 4)
plot(time_sales)

library(TSstudio)
ts_info(time_sales)

#Simple Exponential smoothing model
salesfit <- ses(time_sales,h=3)
summary(salesfit)
autoplot(salesfit)

autoplot(salesfit)+autolayer(fitted(salesfit),series = "Fitted")

#Holts linear exponential smoothing model (double)

sales1 <- window(time_sales,start=c(data2$year[1],4))
salesfit3<-holt(sales1,h=3)
summary(salesfit3)
salesfit3

autoplot(salesfit3)
autoplot(salesfit3)+autolayer(fitted(salesfit3),series = "Fitted")


#Holt Model With Damped Trend
window(time_sales,start = c(data2$year[1],4),end = c(data2$year[87],2)) %>%
  holt(damped = TRUE,H=80,PI = FALSE) %>%
  autoplot()

#HoltWinters Model
sales3 <- window(time_sales,start=c(data2$year[1],4))
salesfit5 <- hw(sales3,seasonal = "additive",h=3)
salesfit6 <- hw(sales3,seasonal = "multiplicative",h=3)

summary(salesfit5)
summary(salesfit6)


library(tidyverse)

autoplot(sales3)+
  autolayer(salesfit5,series = "HW additive forecasts",PI=FALSE)+
  autolayer(salesfit6,series = "HW multiplicative forecasts",PI=FALSE)+
  xlab("Time")+
  ylab("Sales (billions)")+
  ggtitle("US quarterly sales")+
  guides(colour=guide_legend(title="Forecast"))
  

salesfit7 <- ets(time_sales,model = "ZZZ")
summary(salesfit7)
forecast(salesfit7,3)

autoplot(salesfit7)

