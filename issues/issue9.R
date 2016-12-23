library(magrittr)
library(errorlocate)
library(validate)

rules <- validator(a %in% c("A","B"))
data <- data.frame(a="C", x=1)
# goes well
locate_errors(data, rules)

data <- data.frame(a=c(NA, "A"), x=1)
locate_errors(data, rules)

confront(data, rules) %>% summary
