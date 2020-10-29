# Title     : TODO
# Objective : TODO
# Created by: caramel
# Created on: 10/11/20
install.packages('tree')
install.packages('ISLR')
library(tree)
library(ISLR)
data(Carseats)
names(Carseats)
attach(Carseats)
#tree.carseats <- tree(High ~ CompPrice +
#  Income +
#  Advertising +
#  +Population +
#  Price +
#  ShelveLoc +
#  Age +
#  Education +
#  Urban +
#  US, Carseats)
names(Carseats)
#initiate seed
set.seed(1)
par(mar = rep(2, 4))
#initiate training set
train <- sample(1:nrow(Carseats), 200)
tree.carseats <- tree(Sales ~ ., Carseats, subset = train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)

data(Boston)
attach(Boston)

names(Boston)
#crim is the first index.
for(val in names(Boston)[-1]){
  lm.fit<-lm(val)
}

cv.carseats <- cv.tree(tree.carseats)

plot(cv.carseats$size, cv.carseats$dev, type = 'b')
yhat <- predict(tree.carseats, newdata = Carseats[-train,])
carseats.test = Carseats[-train, "Sales"]
plot(yhat, carseats.test)
abline(0, 1)
mean((yhat - Carseats[-train, "Sales"])^2)

#> plot(yhat,carseats.test)
#> abline(0,1)
#> mean((yhat-carseats.test)^2)
#[1] 0.02
