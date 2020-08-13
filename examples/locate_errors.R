rules <- validator( profit + cost == turnover
                  , cost >= 0.6 * turnover # cost should be at least 60% of turnover
                  , turnover >= 0 # can not be negative.
                  )
data <- data.frame(profit=755, cost=125, turnover=200)
le <- locate_errors(data, rules)

print(le)
summary(le)

v_categorical <- validator( A %in% c("a1", "a2")
                          , B %in% c("b1", "b2")
                          , if (A == "a1") B == "b1"
)

data <- data.frame(A = c("a1", "a2"), B = c("b2", "b2"))
locate_errors(data, v_categorical)$errors

v_logical <- validator( A %in% c(TRUE, FALSE)
                      , B %in% c(TRUE, FALSE)
                      ,  if (A == TRUE) B == TRUE
                      )

data <- data.frame(A = TRUE, B = FALSE)
locate_errors(data, v_logical, weight=c(2,1))$errors

# try a condinational rule
v <- validator( married %in% c(TRUE, FALSE), if (married==TRUE) age >= 17 )
data <- data.frame( married = TRUE, age = 16)
locate_errors(data, v, weight=c(married=1, age=2))$errors
