library(validate)
v <- validator(height > 0, weight > 0)

men <- data.frame(height = 5.2, weigth = -50)

cf <- confront(v, men)
