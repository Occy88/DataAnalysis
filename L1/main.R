# Title     : TODO
# Objective : TODO
# Created by: caramel
# Created on: 12/10/2020

Auto <- read.table('Auto.data', header = T, na.strings = '?')
#fix(Auto)
Auto <- na.omit(Auto)
dim(Auto)
#names(Auto)

# ASSIGNMENT 1
# question 1:
# quantitative variables:
# mpg, cylinders,displacement, horsepower, weight,acceleration, year, origin
# qualitative variables:
# name

#question 2 prints range of all quant variables:
print('question 2')
print(length(Auto))
quant <- Auto[-length(Auto)]
for (column in quant) {
  var_range <- range(column)
  var_diff <- diff(var_range)
  var_sd <- sd(column)
  var_mean <- mean(column)
  cat('range: ', var_range, ' | diff: ', var_diff, '\n')
}
#ranges printed are as follows:
#range:  9 46.6  | diff:  37.6
#range:  3 8  | diff:  5
#range:  68 455  | diff:  387
#range:  46 230  | diff:  184
#range:  1613 5140  | diff:  3527
#range:  8 24.8  | diff:  16.8
#range:  70 82  | diff:  12
#range:  1 3  | diff:  2

#question 3:
print('question 3')
for (column in quant) {
  var_range <- range(column)
  var_sd <- sd(column)
  var_mean <- mean(column)
  cat('range: ', var_range, ' | standard_d: ', var_diff, ' | mean: ', var_mean, '\n')
}
#range:  9 46.6  | standard_d:  2  | mean:  23.44592
#range:  3 8  | standard_d:  2  | mean:  5.471939
#range:  68 455  | standard_d:  2  | mean:  194.412
#range:  46 230  | standard_d:  2  | mean:  104.4694
#range:  1613 5140  | standard_d:  2  | mean:  2977.584
#range:  8 24.8  | standard_d:  2  | mean:  15.54133
#range:  70 82  | standard_d:  2  | mean:  75.97959
#range:  1 3  | standard_d:  2  | mean:  1.576531
#question 4:
print('question 4')
for (column in quant) {
  col_reduced <- column[-(10:85)]
  var_range <- range(col_reduced)
  var_sd <- sd(col_reduced)
  var_mean <- mean(col_reduced)
  cat('range: ', var_range, ' | standard_d: ', var_diff, ' | mean: ', var_mean, '\n')
}
#range:  11 46.6  | standard_d:  2  | mean:  24.40443
#range:  3 8  | standard_d:  2  | mean:  5.373418
#range:  68 455  | standard_d:  2  | mean:  187.2405
#range:  46 230  | standard_d:  2  | mean:  100.7215
#range:  1649 4997  | standard_d:  2  | mean:  2935.972
#range:  8.5 24.8  | standard_d:  2  | mean:  15.7269
#range:  70 82  | standard_d:  2  | mean:  77.14557
#range:  1 3  | standard_d:  2  | mean:  1.601266
#question 5:
#plot all quant variables against each other.
#half 8x8 matrix
par(mfrow = c(6, 5))
par(mar = c(2,2,2,2))
col_names <- names(quant)

for (i in 1:length(quant)) {
  for (j in 1:length(quant)) {
    j <- length(quant) + 1 - j
    if (j == i) {
      break
    }
    #cat(col_names[i],'vs',col_names[j],'\n')
    name <- paste(col_names[i], 'vs', col_names[j])
    #print(dim(quant[j]))

    plot(quant[[i]], quant[[j]], xlab = col_names[i], ylab = col_names[j], main = name)
  }
  #plot(0:(length(column) - 1), column,main = '')
}
#All quantitative features have been plotted against each other,
# relationships wehre they exist are obvious, some more apparent than others, such as
# mg vs weight, mpg vs horsepower, displacement vs weight and may more.
#some are linear most follow a more complex relationship.
# some of the relationship have a large varianve which would make predictions difficult such as mpg vs year.

#question 6
# if we wish to predict mpg, then the features that have the greates correlation will work best.
# from the graphs plotted we can observe that weight, horsepower and displacement have the greatest corelation,
# followed by acceleration which would improve accuracy,
# finally year, origin and cylinders, these would be of little help as the error is too high.
