v <- validator( profit + cost == turnover
              , cost - 0.6*turnover >= 0
              , cost>= 0
              , profit >= 0
)
data <- data.frame(profit=755, cost=125, turnover=200)
locate_errors(data, v)

v_categorical <- validator( A %in% c(TRUE, FALSE)
                          , B %in% c(TRUE, FALSE)
                          ,  if (A == TRUE) B == TRUE
                          )

data <- data.frame(A = TRUE, B = FALSE)
locate_errors(data, v_categorical)

# try a condinational rule
v <- validator( married %in% c(TRUE, FALSE), if (married==TRUE) age >= 17 )
data <- data.frame( married = TRUE, age = 16)
locate_errors(data, v)

