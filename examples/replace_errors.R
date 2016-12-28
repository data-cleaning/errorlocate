library(magrittr)

rules <- validator( profit + cost == turnover
              , cost - 0.6*turnover >= 0
              , cost>= 0
              , profit >= 0
)
data <- data.frame(profit=755, cost=125, turnover=200)

data_no_error <-
  data %>%
  replace_errors(rules)

# faulty data was replaced with NA
data_no_error

errors_removed(data_no_error)

# a bit more control
error_locations <- locate_errors(data, rules)
data %>%
  replace_errors(error_locations)
