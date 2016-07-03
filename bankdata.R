table<-read.csv("Lesson 34-Bank Data for Multiple Regression.csv",header=T)
gender<-table[,3]
educlev<-table[,4]
jobgrade<-table[,5]
yrhired<-table[,6]
yrborn<-table[,7]
yrsprior<-table[,8]
salary<-table[,9]
salary = as.numeric(salary) # this command converts all data between commas into numericals

multiRegression1<-lm(formula=salary~gender+educlev+jobgrade+yrhired+yrborn+yrsprior)
summary(multiRegression1)

multiRegression2<-lm(formula=salary~gender+educlev+jobgrade+yrhired+yrsprior)
summary(multiRegression2)

multiRegression3<-lm(formula=salary~gender+educlev+jobgrade+yrhired)
summary(multiRegression3)

multiRegression4<-lm(formula=salary~gender+educlev+jobgrade+yrhired+yrsprior)
summary(multiRegression4)

standard.resid<-rstandard(multiRegression4)  # these are the standardized residuals e* = (y - hat(y))/some constant 
names(multiRegression4)

yhat<-multiRegression4$fitted.values # these are the predicted/fitted values (we obtain these from our final model)
residuals<-multiRegression4$residuals

plot(yhat, standard.resid, main="standardized residuals e* versus yhat", xlab="predicted yhat", ylab="standardized residuals e*", ylim = c(-3,3))
abline(h=c(0,-2,2), lty=3)

plot(gender, standard.resid, main="standardized residuals e* versus gender", xlab = "gender (0 = male, 1 = female)", ylab="standardized residuals e*", ylim=c(-3,3))
abline(h=c(0,-2,2), lty=3)

plot(educlev, standard.resid, main="standardized residuals e* versus education level", xlab = "education level", ylab="standardized residuals e*", ylim=c(-3,3))
abline(h=c(0,-2,2), lty=3)

plot(jobgrade, standard.resid, main="standardized residuals e* versus job grade", xlab = "job grade", ylab="standardized residuals e*", ylim=c(-3,3))
abline(h=c(0,-2,2), lty=3)

plot(yrhired, standard.resid, main="standardized residuals e* versus the year hired", xlab = "the year hired", ylab="standardized residuals e*", ylim=c(-3,3))
abline(h=c(0,-2,2), lty=3)

plot(yrsprior, standard.resid, main="standardized residuals e* versus years of prior experience", xlab = "years of prior experience", ylab="standardized residuals e*", ylim=c(-3,3))
abline(h=c(0,-2,2), lty=3)


plot(salary, yhat, main="yhat (expected salary) vs observed y (actual salary) in thousands", xlab="observed y (actual salary)", ylab="predicted yhat (expected salary)", ylim = c(20,100), xlim = c(20,100))


qqnorm(salary)
qqline(salary)

plot(multiRegression4) 





