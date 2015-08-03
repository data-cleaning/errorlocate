library(validate)
v <- validator(height > 0, weight > 0, x > 0 )

men <- data.frame(height = 5.2, weight = -50, x=NA)
cf <- confront(men, v)
