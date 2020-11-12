R1 <- function(X, s) {
  return(X < s)
}

rss <- function(s, Xj, Y) {
  r1 <- R1(Xj, s)
  yi_r1 <- Y[which(r1 == TRUE)]
  yhat_R1 <- mean(yi_r1)
  yi_r2 <- Y[which(r1 == FALSE)]
  yhat_R2 <- mean(yi_r2)
  return(sum((yi_r1 - yhat_R1)^2) + sum((yi_r2 - yhat_R2)^2))
}


gen_splits_by_step_size <- function(vector, step_size) {
  rg <- range(vector)
  return(seq(rg[1] + step_size, rg[2] - step_size, by = step_size))
}

gen_splits_by_no_splits <- function(vector, no_splits) {
  step_size <- diff(rg) / no_splits
  return(gen_splits_by_step_size(step_size))
}

min_split <- function(X, Y, split_step_sizes) {
  min_rss_vals <- rep(NaN, length(X))
  min_s_vals <- rep(NaN, length(X))
  i <- 1
  for (Xj in X) {
    splits <- gen_splits_by_step_size(Xj, split_step_sizes[i])
    temp_rss <- rep(NaN, length(splits))
    j <- 1
    for (s in splits) {
      temp_rss[j] <- rss(s, Xj, Y)
      j <- j + 1
    }
    min_index <- which.min(temp_rss)
    min_s_vals[i] <- splits[min_index]
    min_rss_vals[i] <- temp_rss[min_index]
    i <- i + 1
  }
  j <- which.min(min_rss_vals)
  s <- min_s_vals[j]
  return(c(j, s))
}

ds_mse <- function(j, s, X, Y) {
  return(rss(s, X[, j], Y) / length(X[, j]))
}


set.seed(1008)
X <- data.frame(Boston['rm'], Boston['lstat'])
Y <- Boston['medv']
split_step_sizes <- rep(0.1, length(X))
train_split_size <- as.integer(length(X[, 1]) / 2)
train_labels <- sample(1:nrow(Y), train_split_size)
Y.train <- Y[train_labels,]
Y.test <- Y[-train_labels,]
X.train <- X[train_labels,]
X.test <- X[-train_labels,]
X.train[2] <- X.train[2]
res <- min_split(X.train, Y.train, split_step_sizes)
ds_mse(res[1], res[2], X.test, Y.test)
plot(Y.train, X.train[, res[1]])
abline(0, 0, res[2])
library(tree)
tree.carseats <- tree(medv ~ rm + lstat, Boston, subset = train_labels)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)
paste0(colnames(X)[res[1]], res[2])
