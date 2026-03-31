x <- c(1, 1, 3, 8)

set.seed(123)
add_noise(x)

m <- rbind(c(1, 2, 3), c(1, 2, 3))
set.seed(123)
add_noise(m, max_delta = 0.05)
