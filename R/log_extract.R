LOGTRANSFORM <- "\\._(log.*)"

log_extract <- function(log_vars){
  log_vars <- log_vars[grepl(LOGTRANSFORM, log_vars)]
  num_vars <- sub(LOGTRANSFORM, "", log_vars)
  m <- regexec(LOGTRANSFORM, log_vars)
  log_fn <- sapply( regmatches(log_vars, m)
                  , function(x){x[[2]]}
                  )
  data.frame(num_vars, log_vars, log_fn)
}

log_derived_data <- function(data, x){
  # assume x was created with log_extract function
  derived_data <- list()
  for (i in seq_len(nrow(x))){
    v <- x$num_vars[i]
    lv <- x$log_vars[i]
    log_fn <- x$log_fn[i]
    # transform the data!
    derived_data[[lv]] <- do.call(log_fn, list(data[[v]]))
  }
  as.data.frame(derived_data)
}

log_constraint_rules <- function(num_var, log_var, logfn, n = 10, r = 1:1e5){
  # TODO check that r[1] >= 1
  r[1] <- max(r[1], 1)

  # sample points based on slope
  value_log <- seq(log(r[1]), log(r[2]), length.out = n)
  value <- exp(value_log)

  # TODO value_log again (to cope with that logfn can be different from "log")
  # upper bound!
  upper <-
    lapply(seq_len(n), function(i){
    a <- c(1, -1/value[i])
    names(a) <- c(log_var, num_var)
    b <- value_log[i] - 1
    rule <- paste0(log_var, ".upperbound_",i)
    mip_rule(a, "<=", b, rule = rule)
  })

  # lowerbound, more nasty...
  lower <- list()
  d_x <- diff(value)
  d_y <- diff(value_log)

  as <- d_y / d_x # check if d_x is zero?
  bs <- value_log[-n] - as * value[-n]

  lower <- structure(
    lapply(seq_len(n-1), function(i){
      substitute( log_var >= a * num_var + b
                , list( a = as[i]
                      , b=bs[i]
                      , log_var = as.symbol(log_var)
                      , num_var = as.symbol(num_var)
                      )
                )
    }), class="dnf"
  )
  lower <- dnf_to_mip_rule(lower, name = paste0(log_var, ".lowerbound"))

  c(upper, lower)
}

# TODO add to  tests
# rules <- validator(log(x) > log(2))
# data <- data.frame(x = 1:3)
# mip <- miprules(rules)
# mip
# le <- log_extract(mip$._vars_num)
# le
# log_enrich_data(data, le)
# log_enrich_data(list(x=1), le)
