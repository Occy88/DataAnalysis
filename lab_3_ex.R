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
Auto['mpg01'] <- mpg01


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

test = Auto[-Auto.train,]
test_vals = mpg01[-Auto.train]
glm.fit <- glm(mpg01 ~ displacement + horsepower + weight + acceleration, data = Auto, family = binomial, subset = Auto.train)
glm.probs <- predict(glm.fit, test, type = "response")
glm.pred <- rep(0, length(test_vals))
glm.pred[glm.probs > .5] <- 1
table(glm.pred, test_vals)
print(mean(glm.pred == test_vals))
print(mean(glm.pred != test_vals))

library(class)
train.X <- cbind(displacement, horsepower, weight, acceleration)[Auto.train,]
test.X <- cbind(displacement, horsepower, weight, acceleration)[-Auto.train,]
train.Y <- mpg01[Auto.train]
test.Y <- mpg01[-Auto.train]
set.seed(1)
for (k in 1:5) {
  knn.pred <- knn(train.X, test.X, train.Y, k = k)
  summary(knn.pred)
  print(table(knn.pred, test.Y))
  print(mean(knn.pred != test.Y))
}

#ifelse(mpg > median(mpg), 1, 0)
data(Boston)
fix(Boston)

X <- Boston[-1]
Y <- Boston[1]

#convert to 0,1 for above/below median.
Y[[1]] <- ifelse(Y[[1]] > median(Y[[1]]), 1, 0)

train_labels <- sample(1:nrow(Y), 200)
Y.train <- Y[train_labels,]
Y.test <- Y[-train_labels,]
X.train <- X[train_labels,]
X.test <- X[-train_labels,]
par(mfrow = c(4, 4))
par(mar = c(2, 2, 2, 2))


for (i in 1:length(X)) {
  #cat(col_names[i],'vs',col_names[j],'\n')
  name <- paste(colnames(Y), 'vs', colnames(X)[i])
  #print(dim(quant[j]))

  plot(X[[i]], Y[[1]], ylab = colnames(Y), xlab = colnames(X)[i], main = name)

  #plot(0:(length(column) - 1), column,main = '')
}


glm.fit <- glm(paste(colnames(Y), '~', paste(colnames(X), collapse = ' + '), sep = ' '), data = cbind(Y, X), family = binomial, subset = train_labels)
glm.probs <- predict(glm.fit, X.test, type = "response")
glm.pred<-ifelse(glm.probs > .5, 1,0)
table(glm.pred, Y.test)
print(mean(glm.pred == Y.test))
print(mean(glm.pred != Y.test))

library(class)

set.seed(1)
for (k in 1:5) {
  knn.pred <- knn(X.train, X.test, Y.train, k = k)
  summary(knn.pred)
  print(table(knn.pred, Y.test))
  print(mean(knn.pred != Y.test))
}