v <- validator( p + c == t
                , c - 0.6*t >= 0
                , c >= 0
                , p >= 0
)
data <- data.frame(p=755, c=125, t=200)
errors <- locate_errors(data, v)
