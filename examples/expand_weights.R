dat <- read.csv(text=
"age,country
  49,     NL
  23,     DE
", strip.white=TRUE)

weight <- c(age = 2, country = 1)
expand_weights(dat, weight)

weight <- c(2, 1)
expand_weights(dat, weight, as.data.frame = TRUE)

# works too
weight <- c(country=5)
expand_weights(dat, weight)

# specify a per row weight for country
weight <- data.frame(country=c(1,5))
expand_weights(dat, weight)

# country should not be changed!
weight <- c(country = Inf)
expand_weights(dat, weight)
