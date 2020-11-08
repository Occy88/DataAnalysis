# Title     : Q1
# Objective : Implement logistic regression algorithm using Gradient Descent in analogy
# with neural networks
# Created by: caramel
# Created on: 07/11/2020

# set a seed (my birthday is 08/10/1997)
seed <- 1008

# previously used Boston to test my results .
#install.packages('MASS')
#library(MASS)

# this function is not used, (as we were asked to use scale() instead)
# I used this for testing my algorithm with Boston dataset.

normalize <- function(x) {
  # Normalize a table ( x - mean(x) )
  for (col_name in colnames(x)) {
    x[col_name] <- x[col_name] - mean(unlist(x[col_name]))
  }
  return(x)
}

# Declare some data for testing:
#data(Boston)
#Boston <- normalize(Boston)
#X <- Boston[-1]
#Y <- Boston[1]

#convert to 0,1 for above/below median.
#Y[[1]] <- ifelse(Y[[1]] > median(Y[[1]]), 1, 0)


# log odds function log( p(x)/1-p(x) )

log_odds <- function(X, B, b0) {
  l_p <- rep(0, length(X[, 1]))
  # X is the feature matrix, B is the parameter vector.
  for (i in 1:length(X[, 1])) {
    x <- X[i,]
    #b0 + dot produt of B and the ith row of X.
    l_p[i] <- b0 + sum(B * x)
  }
  return(l_p)
}

# odds function ( exp(log_odds) )

odds <- function(X, B, b0) {
  return(exp(log_odds(X, B, b0)))
}

# sigmoid function ( odds / (1+odds) )

sigmoid <- function(X, B, b0) {
  # I calculated sigmoid by rearranging from log-odds -> odds -> sigmoid.
  # simply because I am not as comfortable with r yet
  # and doing a dot product was easier for me.
  e_b_x <- odds(X, B, b0)
  return(e_b_x / (1 + e_b_x))
}


# Using MSE

# derivative of xi with respect to b (updating weights)

derivative_wrt <- function(x, b) {
  return(-2 * sum(x * b))
}

# calculate derivatives for given prediction, attribute table and actual labels.

calc_derivatives <- function(y_p, X, Y) {
  b <- (Y - y_p) * (y_p) * (1 - y_p)
  d <- rep(0, length(X) + 1)
  d[1] <- derivative_wrt(1, b) # derivative of loss wrt b0
  for (i in 2:length(X[1,]) + 1) {
    d[i] <- derivative_wrt(X[, i - 1], b) # derivative of loss wrt xi bi
  }
  return(d)
}

# run epochs

logistic_regression <- function(Y, X, train_split_size, learning_rate, epochs) {
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
  for (e in 1:epochs) {
    y_p <- sigmoid(X.train, B, b0)
    d <- calc_derivatives(y_p, X.train, Y.train)
    B <- B - lr * d[-1]
    b0 <- b0 - lr * d[1]
  }
  r_train <- ifelse(sigmoid(X.train, B, b0) > 0.5, 1, 0)
  # print result
  r_test <- ifelse(sigmoid(X.test, B, b0) > 0.5, 1, 0)
  # error
  print("ERROR: ")
  print(mean(r_test != Y.test))
  print("ACCURACY: ")
  print(mean(r_train != Y.train))
  results <- c(mean(r_test != Y.test),mean(r_train != Y.train))
  return(results)

}


