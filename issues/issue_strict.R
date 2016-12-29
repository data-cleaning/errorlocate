library(errorlocate)
library(magrittr)

rules <- validator( if (x < 1) y < 1)
data <- data.frame(x=0, y = 1)

locate_errors(data, rules)$errors
