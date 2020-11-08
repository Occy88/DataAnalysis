# Title     : Q2 -> Q7
# Objective : TODO
# Created by: caramel
# Created on: 08/11/2020
#install.packages('import')
#install.packages('ISLR')
#library(import)
#import::from('./Q1.R', logistic_regression)
source('./A1/Q1.R')
library(ISLR)
data(Auto)
# Question 2
Auto['high'] <- ifelse(Auto['mpg'] > 22 & Auto['mpg'] >= 23, 1, 0)[, 1]

Y <- Auto['high']

# scale quantitative variables
horsepower <- scale(Auto['horsepower'])[, 1]
weight <- scale(Auto['weight'])[, 1]
year <- scale(Auto['year'])[, 1]
X <- data.frame(horsepower, weight, year)

# for origin declare k-1 dummy variables, (ignore first one in the set of unique variables)
for (i in unlist(unique(Auto['origin']))[-1]) {
  print(paste0('origin', i))
  X[paste0('origin', i)] <- ifelse(Auto['origin'] == i, 1, 0)[, 1]
}

logistic_regression(Y, X, 200, 0.001, 20)
#paste0('origin', i)

# now test this on glm from R (logistic regression)
train_labels <- sample(1:nrow(Y), 200)
Y.train <- Y[train_labels,]
Y.test <- Y[-train_labels,]
X.train <- X[train_labels,]
X.test <- X[-train_labels,]
glm.fit <- glm(paste(colnames(Y), '~', paste(colnames(X), collapse = ' + '), sep = ' '), data = cbind(Y, X), family = binomial, subset = train_labels)
glm.probs <- predict(glm.fit, X.test, type = "response")
glm.pred <- ifelse(glm.probs > .5, 1, 0)
table(glm.pred, Y.test)
print(mean(glm.pred == Y.test))
print(mean(glm.pred != Y.test))


# ============ Q 3 ===============
# The dataset has been declared,
# Training and test sets also declared and appropriately scaled (from question 2)
# a global seed has also been declared at the top of the Q1 File
# (will be used throughout the questions


# ============ Q 4 ==============
# The random numbers are already declared between -0.7 -> 0.7 for each weight (see Q1 file)
split_size <- as.integer(length(X[, 1]) / 2)
learning_rate_tests <- c(0.001, 0.002, 0.004, 0.01, 0.04, 0.1, 0.2)
epoch_tests <- c(1, 2, 4, 8, 16, 32, 64)
errors <- logistic_regression(Y, X, split_size, 0.001, 20)
result_length <- length(learning_rate_tests) * length(epoch_tests)
results_table_test <- matrix(0.1, nrow = length(epoch_tests), ncol = length(learning_rate_tests), dimnames = list(epoch_tests, learning_rate_tests))
results_table_train <- matrix(0.1, nrow = length(epoch_tests), ncol = length(learning_rate_tests), dimnames = list(epoch_tests, learning_rate_tests))

for (i in 1:length(epoch_tests)) {
  for (j in 1:length(learning_rate_tests)) {
    errors <- logistic_regression(Y, X, split_size, learning_rate_tests[j], epoch_tests[i])
    results_table_test[i, j] <- errors[1]
    results_table_train[i, j] <- errors[2]
  }
}


#============ Q 5 =====================
# Modify regression function to stop when training mse has less than 1% change over 10 steps.

logistic_regression_1_percent <- function(Y, X, train_split_size, learning_rate) {
  # scale feature matrix:
  train_labels <- sample(1:nrow(Y), train_split_size)

  Y.train <- Y[train_labels,]
  Y.test <- Y[-train_labels,]
  X.train <- X[train_labels,]
  X.test <- X[-train_labels,]

  #learning rate
  lr <- learning_rate

  #  param vector (weights to be updated) -0.7 -> 0.7 (randomly)
  B <- runif(length(X.test), -0.7, 0.7)
  b0 <- 0
  # stopping rule, if value changes by more than 1% over 10 steps keep going otherwise stop.
  steps_since_last_change <- 0
  # some impossible mse that is > 1% of any possible mse (for initial assignment)
  training_mse_last <- 100
  while (steps_since_last_change < 10) {
    y_p <- sigmoid(X.train, B, b0)
    d <- calc_derivatives(y_p, X.train, Y.train)
    B <- B - lr * d[-1]
    b0 <- b0 - lr * d[1]
    r_train <- mean(ifelse(sigmoid(X.train, B, b0) > 0.5, 1, 0))
    r_train_top <- r_train * 1.01
    r_train_bottom <- r_train * 0.99
    if (training_mse_last < r_train_bottom || training_mse_last > r_train_top) {
      training_mse_last <- r_train
      #reset counter
      steps_since_last_change <- 0
    }
    steps_since_last_change <- steps_since_last_change + 1
  }
  r_train <- ifelse(sigmoid(X.train, B, b0) > 0.5, 1, 0)
  # print result
  r_test <- ifelse(sigmoid(X.test, B, b0) > 0.5, 1, 0)
  # error
  print("ERROR: ")
  print(mean(r_test != Y.test))
  print("ACCURACY: ")
  print(mean(r_train != Y.train))
  results <- c(mean(r_test != Y.test), mean(r_train != Y.train))
  return(results)
}

# ======================= Q 6 ========================
# 0.04,8 from table
res <- rep(0, 100)
for (i in 1:length(res)) {
  res[i] <- logistic_regression(Y, X, split_size, 0.04, 8)[1]
}
boxplot(res)

#===================== Q 7 =======================

logistic_regression_1_percent_4time <- function(Y, X, train_split_size, learning_rate) {
  # scale feature matrix:
  train_labels <- sample(1:nrow(Y), train_split_size)

  Y.train <- Y[train_labels,]
  Y.test <- Y[-train_labels,]
  X.train <- X[train_labels,]
  X.test <- X[-train_labels,]

  #learning rate
  lr <- learning_rate

  #  param vector (weights to be updated) -0.7 -> 0.7 (randomly)
  B <- runif(length(X.test), -0.7, 0.7)
  b0 <- 0
  # stopping rule, if value changes by more than 1% over 10 steps keep going otherwise stop.
  steps_since_last_change <- 0
  # some impossible mse that is > 1% of any possible mse (for initial assignment)
  training_mse_last <- 100
  while (steps_since_last_change < 10) {
    min_mse <- 100
    min_p_rule_b0 <- NULL
    min_p_rule_B <- NULL
    for (i in 1:4) {
      y_p <- sigmoid(X.train, B, b0)
      d <- calc_derivatives(y_p, X.train, Y.train)
      B_temp <- B - lr * d[-1]
      b0_temp <- b0 - lr * d[1]
      r_train <- mean(ifelse(sigmoid(X.train, B_temp, b0_temp) > 0.5, 1, 0))
      if (r_train < min_mse) {
        min_mse <- r_train
        min_p_rule_b0 <- b0_temp
        min_p_rule_B <- B_temp
      }
      B <- min_p_rule_B
      b0 <- min_p_rule_b0
    }

    r_train_top <- r_train * 1.01
    r_train_bottom <- r_train * 0.99
    if (training_mse_last < r_train_bottom || training_mse_last > r_train_top) {
      training_mse_last <- r_train
      #reset counter
      steps_since_last_change <- 0
    }
    steps_since_last_change <- steps_since_last_change + 1
  }
  r_train <- ifelse(sigmoid(X.train, B, b0) > 0.5, 1, 0)
  # print result
  r_test <- ifelse(sigmoid(X.test, B, b0) > 0.5, 1, 0)
  # error
  print("ERROR: ")
  print(mean(r_test != Y.test))
  print("ACCURACY: ")
  print(mean(r_train != Y.train))
  results <- c(mean(r_test != Y.test), mean(r_train != Y.train))
  return(results)
}

print(logistic_regression_1_percent_4time(Y, X, split_size, 0.04))