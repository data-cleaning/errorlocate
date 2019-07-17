library(errorlocate)
library(validate)

df <- data.frame(a=-1, b=0)
rules <- validator(var_group(a,b)>=0)
rules

# not expanding var_group
replace_errors(df, rules)

rules <- validator(G := var_group(a,b), G >=0)
replace_errors(df, rules)
