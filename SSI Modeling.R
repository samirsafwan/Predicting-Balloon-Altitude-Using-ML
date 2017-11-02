setwd("~/Downloads")
data=read.csv("ssi54-3.csv")


# GBM
library(gbm)
set.seed(1)
boost.mse=rep(0,19)
predicted.trajectory=vector()
for (i in 1:19) {
  sample=data[1:(1396*i),]
  test=data[(1396*i+1):(1396*(i+1)),]
  boost.fit=gbm(altitude~., data=sample, distribution='gaussian', n.trees=2000, shrinkage=0.1, cv.folds=5)
  boost.pred=predict(boost.fit, test, n.trees=gbm.perf(boost.fit, method='cv'))
  predicted.trajectory=c(predicted.trajectory, boost.pred)
  boost.mse[i]=mean((boost.pred-test$altitude)^2)
}
boost.mse # mean mse = 414,710
plot(data$time, data$altitude, type='l')
lines(data[1397:27920,]$time, predicted.trajectory, type='l', col='red')
for (i in 1:18) {
  abline(v=data[1396*i:1396*i,]$time, col='gray')
}

# Without temp
library(gbm)
set.seed(1)
boost.mse2=rep(0,19)
predicted.trajectory2=vector()
for (i in 1:19) {
  sample=data[1:(1396*i),]
  test=data[(1396*i+1):(1396*(i+1)),]
  boost.fit=gbm(altitude~.-temperature, data=sample, distribution='gaussian', n.trees=2000, shrinkage=0.1, cv.folds=5)
  boost.pred=predict(boost.fit, test, n.trees=gbm.perf(boost.fit, method='cv'))
  predicted.trajectory2=c(predicted.trajectory2, boost.pred)
  boost.mse2[i]=mean((boost.pred-test$altitude)^2)
}
boost.mse2 # mean mse = 3,415,947
plot(data$time, data$altitude, type='l')
lines(data[1397:27920,]$time, predicted.trajectory2, type='l', col='red')
for (i in 1:18) {
  abline(v=data[1396*i:1396*i,]$time, col='gray')
}

# Lasso
library(glmnet)
set.seed(1)
lasso.mse=rep(0,19)
for (i in 1:19) {
  sampleX=as.matrix(data[1:(1396*i),1:12])
  sampleY=as.matrix(data[1:(1396*i),]$altitude)
  testX=as.matrix(data[(1396*i+1):(1396*(i+1)),1:12])
  lasso.fit=cv.glmnet(sampleX, sampleY, alpha=1)
  lasso.pred=predict(lasso.fit, s=lasso.fit$lambda.min, newx=testX)
  lasso.mse[i]=mean((lasso.pred-data[(1396*i+1):(1396*(i+1)),]$altitude)^2)
}
lasso.mse # mean mse = 3,415,296


# GAM with Splines
library(splines)
library(gam)
set.seed(1)
gam.mse=rep(0,19)
for (i in 1:19) {
  sample=data[1:(1396*i),]
  test=data[(1396*i+1):(1396*(i+1)),]
  gam.fit=gam(altitude~s(time)+s(temperature), data=sample)
  gam.pred=predict(gam.fit, test)
  gam.mse[i]=mean((gam.pred-test$altitude)^2)
}
gam.mse # mean mse = 718,245
