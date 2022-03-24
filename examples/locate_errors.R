rules <- validator( profit + cost == turnover
                  , cost >= 0.6 * turnover # cost should be at least 60% of turnover
                  , turnover >= 0 # can not be negative.
                  )
data <- data.frame( profit   = 755
                  , cost     = 125
                  , turnover = 200
                  )
le <- locate_errors(data, rules)

print(le)
summary(le)

v_categorical <- validator( branch %in% c("government", "industry")
                          , tax %in% c("none", "VAT")
                          , if (tax == "VAT") branch == "industry"
)

data <- read.csv(text=
"   branch, tax
government, VAT
industry  , VAT
", strip.white = TRUE)
locate_errors(data, v_categorical)$errors

v_logical <- validator( citizen %in% c(TRUE, FALSE)
                      , voted %in% c(TRUE, FALSE)
                      ,  if (voted == TRUE) citizen == TRUE
                      )

data <- data.frame(voted = TRUE, citizen = FALSE)
locate_errors(data, v_logical, weight=c(2,1))$errors

# try a condinational rule
v <- validator( married %in% c(TRUE, FALSE)
              , if (married==TRUE) age >= 17
              )
data <- data.frame( married = TRUE, age = 16)
locate_errors(data, v, weight=c(married=1, age=2))$errors


# different weights per row
data <- read.csv(text=
"married, age
    TRUE,  16
    TRUE,  14
", strip.white = TRUE)

weight <- read.csv(text=
"married, age
       1,   2
       2,   1
", strip.white = TRUE)

locate_errors(data, v, weight = weight)$errors

# fixate / exclude a variable from error localiziation
# using an Inf weight
weight <- c(age = Inf)
locate_errors(data, v, weight = weight)$errors
