#LOADING THE DATA
given_data<-read.csv("credit_v2.csv")

#ANALYSIS OF THE GIVEN DATA
cor(given_data)

library(psych)
describe(given_data)

pairs(given_data[,1:9],pch = 19,panel = panel.smooth)

library("PerformanceAnalytics")
chart.Correlation(given_data, histogram=TRUE, pch=19)

cor(given_data,method = "pearson")

#Trial model and Removing independent variables with highest p value(BACKWARD ELIMINATION)

trial_fit<-lm(creddebt~ï..age +ed+employ+address+income+debtinc+othdebt+default,data=given_data)
summary(trial_fit)

trial_fit2<-lm(creddebt~ï..age+employ+address+income+debtinc+othdebt+default,data=given_data)
summary(trial_fit2)

trail_fit3<-lm(creddebt~employ+address+income+debtinc+othdebt+default,data=given_data)
summary(trail_fit3)

trail_fit4<-lm(creddebt~employ+income+debtinc+othdebt+default,data=given_data)
summary(trail_fit4)

trial_fit5<-lm(creddebt~employ+income+debtinc+default,data=given_data)
summary(trial_fit5)

#FIRST MODEL
real_fit<-lm(creddebt~employ+income+debtinc+default,data=given_data)
summary(real_fit)
#Model Visualization of real_fit
par(mfrow =c(2,2))
plot(real_fit)


#Taking log of 'creddebt' for homosedasticity
given_data$creddebt<-log10(given_data$creddebt)

#UPDATED MODEL-1(Replacing 'Default' by 'Othdebt'as the p value was  increased on taking log of Creddebt)
update_fit1<-lm(creddebt~employ+income+debtinc+othdebt,data=given_data)
summary(update_fit1)
#Model Visualization of update_fit1
par(mfrow =c(2,2))
plot(update_fit1)

#Transforming indepependent variables
given_data$employ<-sqrt(given_data$employ)
given_data$income<-sqrt(given_data$income)
#cor(given_data,method = "pearson")
given_data$debtinc<-log10(given_data$debtinc)
given_data$othdebt<-log10(given_data$othdebt)

#Updated Model -3 (Droping Income)
update_fit2<-lm(creddebt~employ+
                debtinc+othdebt,
                data=given_data)
summary(update_fit2)
#Model Visualization of update_fit2
par(mfrow =c(2,2))
plot(update_fit2)

#Alternate Model (Droping Employ)
alternate_fit1<-lm(creddebt~income+debtinc+othdebt,data=given_data)
summary(alternate_fit1)
#Model Visualization of alternate_fit1
par(mfrow =c(2,2))
plot(alternate_fit1)

#Updating Alternate model (Dropping 'otherdebt')
alternate_fit2<-lm(creddebt~income+debtinc,data=given_data)
summary(alternate_fit2)
#Model Visualization of update_fit2
par(mfrow =c(2,2))
plot(alternate_fit2)

#REMOVING OUTLIERS
Q1 <- quantile(given_data$debtinc, .25)
Q3 <- quantile(given_data$debtinc, .75)
IQR <- IQR(given_data$debtinc)
given_data <- subset(given_data,
                     given_data$debtinc > 
                       (Q1 - 1.5*IQR)
                     & given_data$debtinc
                     < (Q3 + 1.5*IQR))
boxplot(given_data$creddebt)

#ASSUMPTION 1 : Linearity Assumption

plot(update_fit3, which=1, col=c("black"))

#ASSUMTION 2 : Errors have constatnt variance(homosedasticity)

plot(update_fit3, which=3, col=c("black"))
library(car)
ncvTest(alternate_fit2)

#ASSUMPTION 3 : No Autocorrelation between errors / Independence of errors
durbinWatsonTest(alternate_fit2)

#ASSUMPTION 4: ERRORS are normally distributed
plot(update_fit2, which=2, col=c("black"))

#ASSUMPTION 5 : Absence of multicollinearity
cor(given_data,method = "pearson")
vif(alternate_fit2)

#ASSUMPTION 6 : No influential data points
plot(update_fit3, which=5, col=c("black"))
cooks.distance(alternate_fit2)
influencePlot(model = alternate_fit2,scale = 3,main = "Influence Plot")

#Histogram of the residuals - Upfdate_fit3
library(ggplot2)
ggplot(data = alternate_fit2, aes(x = alternate_fit2$residuals)) +
  geom_histogram(bins = 30,fill = 'steelblue', color = 'black') + 
  #stat_function(fun = dnorm, args = list(mean = mean(cred_data2$creddebt), sd = sd(cred_data2$creddebt)))
  labs(title = 'Histogram of Residuals', x = 'Residuals', y = 'Frequency')
#Density Curve of the residuals
resi<-residuals(update_fit2)
plot(density(resi))

residuals <- resid(alternate_fit2)

plot(fitted(alternate_fit2),residuals)

abline(0,0)


hist(alternate_fit2$residuals, freq = FALSE)
x<-seq(-4,+4,by=0.02)
curve(dnorm(x), add=TRUE)


residualPlot(alternate_fit2)

library(FSA)

residPlot(alternate_fit2)

install.packages("FSA")