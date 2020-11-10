#=================== q1 ==============

Power <- function() {
  x <- 2^3
  print(x)
  return(x)
}

#================= q2 =======================

Power2 <- function(a, b) {
  x <- a^b
  print(x)
  return(x)
}

#================= q3 =======================
Power2(10, 3)
Power2(8, 17)
Power2(131, 3)

#================= q4 =======================

Power3 <- Power2
print(Power3(4, 2))

#================= q5 =======================
x <- 1:10
y <- Power3(x, 2)
par(mfrow = c(2, 1))

plot(x, y, xlab = '1->10', ylab = '(1->10)^2')
plot(x, y, log = 'y', xlab = '1->10', ylab = '(1->10)^2')

#================= q6 =======================

PlotPower <- function(x, power) {
  plot(x, Power3(x, power), xlab = 'x', ylab = paste0('x^', power))
}

PlotPower(1:10, 3)

#=================== q7 ===============

sum_p <- function(n, power) {
  return(sum(Power3(1:n, power)))
}

sum2 <- function(n) {
  return(sum_p(n, 2))
}

sum2(3)

#==================== q8 ===============

sum3 <- function(n) {
  return(sum_p(n, 3))
}

sum3(3)

#==================== q9 ====================

# -1:  My only reasoning is that the dataset is small,values are random throughout the
# whole dataset, hence data snooping will make a very small difference
# in the results for this case.

# -2:
library(ISLR)
library(MASS)
X <- Caravan[, -86]
Y <- Caravan[, 86]
test <- 1:1000
X.train <- scale(X[-test,])
X.test <- scale(X[test,],
                center = attr(X.train, 'scaled:center'),
                scale = attr(X.train, 'scaled:scale'))
Y.train <- Y[-test]
Y.test <- Y[test]
set.seed(1)
knn.pred1 <- knn(X.train, X.test, Y.train, k = 1)
knn.pred3 <- knn(X.train, X.test, Y.train, k = 3)
knn.pred5 <- knn(X.train, X.test, Y.train, k = 5)

mean(Y.test != knn.pred1)
# 0.117
mean(Y.test != "No")
# 0.059
table(knn.pred1, Y.test)
# 9/76 = 0.118421053

table(knn.pred3, Y.test)
# 6/20 = 0.2308
table(knn.pred5, Y.test)
# 3/13 = 0.2308

# difference from the lab is not siginificant.
