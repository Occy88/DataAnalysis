# Title     : TODO
# Objective : TODO
# Created by: caramel
# Created on: 27/10/2020

library(MASS)
library(ISLR)

data(Boston)
attach(Boston)
lm.fit <- lm(medv ~ lstat)
lm.fit

names(lm.fit)
predict(lm.fit, data.frame(lstat = c(5, 10, 15)))

plot(lstat, medv)
abline(lm.fit)

abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")
plot(lstat, medv, col = "red")
plot(lstat, medv, pch = 20)
plot(lstat, medv, pch = "+")
plot(1:20, 1:20, pch = 1:20)

lm.fit <- lm(medv ~ lstat + age, data = Boston)
lm.fit

lm.fit <- lm(medv ~ ., data = Boston)
lm.fit

names(Carseats)

lm.fit <- lm(Sales ~ ., data = Carseats)
lm.fit

attach(Carseats)
contrasts(ShelveLoc)

names(Smarket)
summary(Smarket)

attach(Smarket)
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,data = Smarket, family = binomial)
glm.fit

coef(glm.fit)

glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:6]

contrasts(Direction)

glm.pred <- rep("Down", 1250)
glm.pred[glm.probs >.5] <- "Up"

table(glm.pred, Direction)

mean(glm.pred == Direction)

train <- (Year < 2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)

Direction.2005 <- Direction[!train]

glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,data = Smarket, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")

glm.pred <- rep("Down", 252)
glm.pred[glm.probs >.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial,subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs >.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

predict(glm.fit, newdata = data.frame(Lag1 = c(1.2, 1.5),Lag2 = c(1.1, -0.8)), type = "response")


#  KNN
library(class)
train.X <- cbind(Lag1, Lag2)[train,]
test.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)

knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
