
# THE ADVERTISING DATASET

Advertising <- read.csv("Advertising.csv")
attach(Advertising)

plot(TV, sales, pch=20)


# sample size
n <- length(sales)
n

#################
# PREDICTION 
#################

plot(TV, sales, pch=20)
mod.out <- lm(sales~TV)
summary(mod.out)

abline(mod.out, lwd=2)

coefficients(mod.out)

7.03259355+0.04753664*170 
points(170, 15.114, pch="X", col="red")

new.x <- data.frame(TV=170)
predict(mod.out, newdata=new.x)

predict(mod.out, newdata=new.x, interval ="confidence")
predict(mod.out, newdata=new.x, interval ="prediction")

# adding confidence and prediction intervals in the plot

range(TV)

xp <- seq(1, 295, length=100)
new.x <- data.frame(TV=xp)
new.conf <- predict(mod.out, newdata=new.x, interval = "confidence")

lines(xp, new.conf[,2], lty=2, col="red", lwd=2)
lines(xp, new.conf[,3], lty=2, col="red", lwd=2)

new.pred <- predict(mod.out, newdata=new.x, interval = "prediction")
lines(xp, new.pred[,2], lty=2, col="blue", lwd=2)
lines(xp, new.pred[,3], lty=2, col="blue", lwd=2)

###################################
# DIAGNOSTICS AND RESIDUAL PLOTS
###################################

plot(TV, sales, pch=20)
mod.out <- lm(sales~TV)
summary(mod.out)

# residual plot with covariate "TV" on the x-axis 
#
par(mfrow=c(1,2))
plot(TV, sales)
abline(mod.out, col="blue", lwd=2)
plot(TV, residuals(mod.out), col="gray40", xlab="TV", ylab="residuals")
lines(loess.smooth(TV, residuals(mod.out)), col="blue", lwd=2)
abline(h=0, lty=2)
par(mfrow=c(1,1))

# residual plot with fitted.values on the x-axis 
#
par(mfrow=c(1,2))
plot(TV, sales)
abline(mod.out, col="blue", lwd=2)
plot(fitted(mod.out), residuals(mod.out), col="gray40", xlab="fitted values", ylab="residuals")
lines(loess.smooth(fitted(mod.out), residuals(mod.out)), col="blue", lwd=2)
abline(h=0, lty=2)
par(mfrow=c(1,1))

# residuals plots in R

plot(mod.out)

par(mfrow=c(2,2))
plot(mod.out)
par(mfrow=c(1,1))

#
# AUTO DATASET
#

library(ISLR2)
data(Auto)
attach(Auto)
mod.out <- lm(mpg~horsepower)

summary(mod.out)

par(mfrow=c(1,2))
plot(horsepower, mpg)
abline(mod.out, col="blue", lwd=2)
plot(fitted(mod.out), residuals(mod.out), col="gray40", xlab="fitted values", ylab="residuals")
lines(loess.smooth(fitted(mod.out), residuals(mod.out)), col="blue", lwd=2)
abline(h=0, lty=2)
par(mfrow=c(1,1))


# try to use this model for prediction 

plot(horsepower, mpg, pch=16)
abline(mod.out, lwd=2)
xp <- c(60, 130, 200)
new.x <- data.frame(horsepower=xp)
new.y <- predict(mod.out, newdata=new.x)
points(xp, new.y, pch=17, col="red", cex=1.4)

plot(fitted.values(mod.out), residuals(mod.out), pch=20)
abline(h=0)

# polynomial regression 

mod.out2 <- lm(mpg  ~ horsepower + I(horsepower^2))
summary(mod.out2)

# plot the fitted model 

plot(horsepower, mpg, pch=20)
range(horsepower)
xp <- seq(46, 230, length=100)
new.x <- data.frame(horsepower=xp) 
yp <- predict(mod.out2, newdata=new.x)

lines(xp, yp, lwd=2, col="red")


par(mfrow=c(2,2))
plot(mod.out2)
par(mfrow=c(1,1))


# polynomial of degree 5

mod.out.poly <- lm(mpg~poly(horsepower, 5, raw=TRUE))

summary(mod.out.poly)

range(horsepower)
xp <- seq(46, 230, length=100)
new.x <- data.frame(horsepower=xp) 
yp <- predict(mod.out.poly, newdata=new.x)
plot(horsepower, mpg, pch=20)
lines(xp, yp, lwd=2, col="blue")

###################################
# LOG-TRANSFORM OF RESPONSE
###################################

plot(horsepower, log(mpg), pch=20)

mod.out3 <- lm(log(mpg)  ~ horsepower)

summary(mod.out3)
abline(mod.out3, lwd=2)

plot(fitted.values(mod.out3), residuals(mod.out3), pch=20)
abline(h=0)


range(horsepower)
xp <- seq(46, 230, length=100)
new.x <- data.frame(horsepower=xp) 
yp <- predict(mod.out3, newdata=new.x)

plot(horsepower, mpg, pch=20)
lines(xp, exp(yp), lwd=2, col="blue")

#####################################################
# MULTIPLE REGRESSION: LOG-TRANSFORM OF THE RESPONSE
#####################################################
mod.out <- lm(mpg ~ horsepower + weight)
summary(mod.out)

new.x <- data.frame(weight=2000, horsepower=125)
predict(mod.out, newdata=new.x, interval="confidence")

plot(fitted.values(mod.out), residuals(mod.out), pch=20)
abline(h=0)

mod.out2 <- lm(log(mpg) ~ horsepower + weight)
summary(mod.out2)

plot(fitted.values(mod.out2), residuals(mod.out2), pch=20)
abline(h=0)

y.pred <- predict(mod.out2, newdata=new.x, interval="confidence")

exp(y.pred)


##################################
# EXAMPLE OF POLYNOMIAL REGRESSION 
# mcycle dataset
# use of the function anova
##################################

library(MASS)
data(mcycle)
attach(mcycle)


####

plot(times, accel, xlab="Time", ylab="Acc.", pch=20)


####

times.st <- times - mean(times)


####

pol15 <- lm(accel~times.st+I(times.st^2)+I(times.st^3)
            +I(times.st^4)+I(times.st^5)+ I(times.st^6)
            +I(times.st^7)+I(times.st^8)+I(times.st^9)
            +I(times.st^10)+I(times.st^11)+I(times.st^12)
            +I(times.st^13)+I(times.st^14)+I(times.st^15))


anova(pol15)


####

pol12 <- lm(accel~times.st+I(times.st^2)+I(times.st^3)
            +I(times.st^4)+I(times.st^5)+I(times.st^6)
            +I(times.st^7)+I(times.st^8)+I(times.st^9)
            +I(times.st^10)+I(times.st^11)+I(times.st^12))

summary(pol12)

par(mfrow=c(2,2))
plot(pol12)
par(mfrow=c(1,1))


####

plot(times, accel)
lines(times, pol12$fitted)
####

pred <- predict(pol12, data.frame(times.st=20:30-mean(times)),
                interval="prediction", level=0.95)
pred
points(20:30, pred[,1], col="red")

