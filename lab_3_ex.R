# Title     : TODO
# Objective : TODO
# Created by: caramel
# Created on: 27/10/2020

library(MASS)
library(ISLR)
data(Auto)
attach(Auto)
lm.fit <- lm(mpg ~ horsepower)
#
names(lm.fit)
print("horsepower prediction:")

#
plot(horsepower, mpg, xlab = 'horsepower', ylab = 'mpg', pch = '+')
abline(lm.fit, col = 'red')
title(main = "Horsepower vs mpg Auto Dataset",
      xlab = "horsepower", ylab = "mpg")
predict(lm.fit, data.frame(horsepower = 98))

# ex 2
data(Boston)
attach(Boston)
names(Boston)
#crim is the first index.
all_vals <- list()
for (val in names(Boston)[-1]) {
  #print(paste0('crim ~', paste0(val), collapse = '+'))
  print("-----------evaluating: ----------")
  print(paste('crim ~ ', val, sep = ' '))
  lm.fit <- lm(paste('crim ~', val, sep = ' '), data = Boston)
  coeff <- lm.fit['coefficients'][[1]]
  all_vals[val] <- coeff[val]
}

#  now for all attributes
lm.fit <- lm(crim ~ ., data = Boston)
coeff <- lm.fit['coefficients'][[1]][-1]
plot(all_vals, coeff, xlab = '2a (individual)', ylab = '2b (combined)')
text(all_vals, coeff, names(Boston)[-1], cex = 0.6, pos = 4, col = 'blue')

lm.fit <- lm(crim ~ ., data = Boston)

# ex 3
data(Auto)
names(Auto)
attach(Auto)
mpg01 <- ifelse(mpg > median(mpg), 1, 0)
Auto['mpg01']<-mpg01


#explore graphically:
par(mfrow = c(3, 3))
quant <- Auto[-length(Auto)]

col_names <- names(quant)

for (i in 1:length(quant)) {
  #cat(col_names[i],'vs',col_names[j],'\n')
  name <- paste('mpg01', 'vs', col_names[i])
  #print(dim(quant[j]))

  plot(mpg01, quant[[i]], xlab = 'mpg01', ylab = col_names[i], main = name)

  #plot(0:(length(column) - 1), column,main = '')
}
Auto.train <- sample(1:nrow(Auto), 200)

test=mpg01[-Auto.train]
glm.fit<-glm(mpg01~displacement+horsepower+weight+acceleration,data=Auto,family=binomial,subset=Auto.train)

#abline(lm.fit, lwd = 3, col = "red")
#plot(lstat, medv, col = "red")
#plot(lstat, medv, pch = 20)
#plot(lstat, medv, pch = "+")
#plot(1:20, 1:20, pch = 1:20)
#
#lm.fit <- lm(medv ~ lstat + age, data = Boston)
#lm.fit
#
#lm.fit <- lm(medv ~ ., data = Boston)
#lm.fit
#
#fix(Carseats)
#names(Carseats)
#
#lm.fit <- lm(Sales ~ ., data = Carseats)
#lm.fit
#
#attach(Carseats)
#contrasts(ShelveLoc)
#
#names(Smarket)
#summary(Smarket)
#
#attach(Smarket)
#glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,data = Smarket, family = binomial)
#glm.fit
#
#coef(glm.fit)
#
#glm.probs <- predict(glm.fit, type = "response")
#glm.probs[1:6]
#
#contrasts(Direction)
#
#glm.pred <- rep("Down", 1250)
#glm.pred[glm.probs >.5] <- "Up"
#
#table(glm.pred, Direction)
#
#mean(glm.pred == Direction)
#
#train <- (Year < 2005)
#Smarket.2005 <- Smarket[!train,]
#dim(Smarket.2005)
#
#Direction.2005 <- Direction[!train]
#
#glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,data = Smarket, family = binomial, subset = train)
#glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
#
#glm.pred <- rep("Down", 252)
#glm.pred[glm.probs >.5] <- "Up"
#table(glm.pred, Direction.2005)
#mean(glm.pred == Direction.2005)
#mean(glm.pred != Direction.2005)
#
#glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial,subset = train)
#glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
#glm.pred <- rep("Down", 252)
#glm.pred[glm.probs >.5] <- "Up"
#table(glm.pred, Direction.2005)
#mean(glm.pred == Direction.2005)
#
#predict(glm.fit, newdata = data.frame(Lag1 = c(1.2, 1.5),Lag2 = c(1.1, -0.8)), type = "response")
#
#
##  KNN
#library(class)
#train.X <- cbind(Lag1, Lag2)[train,]
#test.X <- cbind(Lag1, Lag2)[!train,]
#train.Direction <- Direction[train]
#
#set.seed(1)
#knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
#table(knn.pred, Direction.2005)
#
#knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
#table(knn.pred, Direction.2005)
