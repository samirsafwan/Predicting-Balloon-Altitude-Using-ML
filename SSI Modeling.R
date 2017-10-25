setwd("~/Downloads")
data=read.csv("ssi54-2.csv")


# "time", "altitud_barometer", "lat_gps", "long_gps", "valve_time_total", 
# "ballast_time_total"

# decision tree approach

# make training and test sets
set.seed(1)
s1=sample(5504598, 1000000)
train1=data[-s1,]
test1=data[s1,]

library(tree)
library(gbm)
set.seed(1)
tree.fit=tree(altitude_barometer~.-time, data=train1)
plot(tree.fit)
text(tree.fit, pretty=0)
tree.pred=predict(tree.fit, test1)
mean((tree.pred-test1$altitude_barometer)^2)

library(gbm)
set.seed(1)
boost.fit=gbm(altitude_barometer~.-time, data=train1, distribution='gaussian', n.trees=500, shrinkage=0.1)
boost.pred=predict(boost.fit, test1, n.trees=500)
mean((boost.pred-test1$altitude_barometer)^2)

library(splines)
spline.fit1=smooth.spline(train1$altitude_barometer, train1$lat_gps, cv=TRUE)
lims=range(train1$lat_gps)
grid=seq(from=lims[1], to=lims[2])
pred=predict(spline.fit1, newdata=list(grid), se=TRUE)
plot(train1$lat_gps, train1$altitude_barometer)
lines(grid, pred$fit, lwd=2, col='blue')



