given_data <- read.csv("House Categories.csv",header=T, na.strings=c(""), stringsAsFactors = T)
str(given_data)

given_data$X <- NULL

str(given_data)

library(psych)
library(haven)


numeric_mat<-given_data[,1:9]

cor(numeric_mat,method = "pearson")

pairs(given_data[,1:9],pch = 19,panel = panel.smooth)

library("PerformanceAnalytics")
chart.Correlation(given_data[,1:9], histogram=TRUE, pch=19)

library(psych)

describe(given_data)

unique(given_data$fuel)
unique(given_data$waterfront)
unique(given_data$newConstruction)


describe(given_data)


KMO(given_data[1:9])

bartlett.test(given_data[1:9])


fa.parallel(given_data[1:9],fa="pc",n.iter = 100)

pc.given_data<-principal(given_data[1:9],nfactors = 3,rotate = "none")
pc.given_data

rc.given_data<-principal(given_data[1:9],nfactors = 3,rotate="varimax")
rc.given_data

fa.diagram(rc.given_data)

rc_scores<-rc.given_data$score

given_data2<-data.frame(rc_scores,given_data[,10:13])

#Assumption 1 : The dependent variable must be dichotomous variable
given_data2$PriceCat<-as.factor(given_data2$PriceCat)


price_fit1 <- glm(PriceCat~., family = "binomial", data=given_data2)

summary(price_fit1)


price_fit2 <- update(price_fit1,~.-fuel)
summary(price_fit2)

price_fit3 <- update(price_fit2,~.-newConstruction)
summary(price_fit3)

coef(price_fit3)
exp(coef(price_fit3))
exp(coef(price_fit3))*100

exp(price_fit3$coefficients[-1])

(exp(price_fit3$coefficients[-1])-1)*100

library(ResourceSelection)
hoslem.test(price_fit3$y,fitted(price_fit3))

#PseudoRSquared Statistics
library(DescTools)
PseudoR2(price_fit3, which = "all")

library(caret)

pred1<-predict(price_fit3, type="response")

pred2<-ifelse(pred1<0.5, "1","2")

confusionMatrix(as.factor(pred2),as.factor(given_data2$PriceCat),positive = "2")

#Assumption2 : Absence of Multicollinearity

library(car)
vif(price_fit3)

#Assumption 3 : No influential data points
rstandard(price_fit3)
cooks.distance(price_fit3)
influencePlot(model = price_fit3,scale = 3,main = "Influence Plot")

#Assumption 4 : Independence of Observations
durbinWatsonTest(price_fit3)

#ASSUMPTION 4
linear_fit<-glm(PriceCat~RC1+log(RC1)*RC1+RC2+log(RC2)*RC2+RC3+log(RC3)*RC3,family = "binomial", data=given_data2)
summary(linear_fit)

linear_fit3<-glm(PriceCat~RC1*log(RC1)+RC2*log(RC2)+RC3*log(RC3),family = "binomial", data=given_data2)
summary(linear_fit3)


linear_fit3<-glm(PriceCat~RC1+RC1:log(RC1)+RC2+RC2:log(RC2)+RC3+RC3:log(RC3),family = "binomial", data=given_data2)
summary(linear_fit2)


library(ROCR)
rc_pred <- predict(price_fit3, newdata=given_data2, type="response")
rc_pr <- prediction(rc_pred, given_data2$PriceCat)
rc_prf <- performance(rc_pr, measure = "tpr", x.measure = "fpr",auc=TRUE)
plot(rc_prf,main = "ROC Curve",col = 1,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")

auc1 <- performance(rc_pr, measure = "auc")
auc1 <- auc1@y.values[[1]]
auc1

