LOGTRANSFORM <- "\\._(log.*)"

log_extract <- function(log_vars){
  log_vars <- log_vars[grepl(LOGTRANSFORM, log_vars)]
  num_vars <- sub(LOGTRANSFORM, "", log_vars)
  m <- regexec(LOGTRANSFORM, log_vars)
  log_fn <- sapply( regmatches(log_vars, m)
                  , function(x){x[[2]]}
                  )
  data.frame(num_vars, log_vars, log_fn, stringsAsFactors = FALSE)
}

# create data columns used to set values for log transformed variables.
log_derived_data <- function(data, x, eps=1e-7){
  # assume x was created with log_extract function
  derived_data <- list()
  for (i in seq_len(nrow(x))){
    v <- x$num_vars[i]
    lv <- x$log_vars[i]
    log_fn <- x$log_fn[i]
    # transform the data!
    # precaution to prevent Inf
    d <- data[[v]]
    d[d<=eps] <- eps
    derived_data[[lv]] <- do.call(log_fn, list(d))
  }
  as.data.frame(derived_data)
}

create_log_constraints <- function(li, data = NULL, n = 10, r = c(1,1e5)){
  lc <- lapply(seq_len(nrow(li)), function(i){
    num_var <- li$num_vars[i]
    if (!is.null(data[[num_var]])){
      r2 <- range(data[[num_var]], na.rm = TRUE)
      if (all(is.finite(r2))){
        r <- r2
      }
    }
    log_constraint_rules( li$num_vars[i]
                        , li$log_vars[i]
                        , li$log_fn[i]
                        , n = n
                        , r = r
                        )
  })

  if (length(lc)){ # unlist of empty list is NULL
    lc <- unlist(lc, recursive = FALSE)
  }
  lc
}

log_constraint_rules <- function( num_var, log_var, logfn, n = 10, r = c(1,1e5)
                                , d = NULL
                                ){
  stopifnot(n > 1)
  r[1] <- max(r[1], 1)
  if (r[1] >=  r[2]){
    r <- c(r[1]/10, 10*r[1])
  }

  # retrieve log function
  log_f <- get(logfn)
  exp_f <- switch( logfn
                 , log10 = function(x){10^x}
                 , log1p = expm1
                 , exp
                 )

  # sample points based on slope
  value_log <- seq(log_f(r[1]), log_f(r[2]), length.out = n)
  value_log <- unique(sort(c(value_log, d)))

  value <- exp_f(value_log) # log distributed points covering range.
  f_value <- value_log

  # create points
  points <- paste0(num_var, "._x", sprintf("%03d", seq_len(n)))

  # function rule:
  a <- c(-1, f_value)
  names(a) <- c(log_var, points)

  func_rule <- mip_rule( a = a
                    , op = "=="
                    , b = 0
                    , rule = paste0(log_var, ".", "func"))
  # ref rule:
  a <- c(-1, value)
  names(a) <- c(num_var, points)
  ref_rule <- mip_rule( a = a
                         , op = "=="
                         , b = 0
                         , rule = paste0(log_var, ".", "ref"))

  # convex rule:
  a <- rep(1, length(points))
  names(a) <- points
  cvx_rule <- mip_rule( a = a
                       , op = "=="
                       , b = 1
                       , rule = paste0(log_var, ".", "cvx"))

  list(func_rule, ref_rule, cvx_rule)
}

# TODO add to  tests
# rules <- validator(log(x) > log(2))
# data <- data.frame(x = 1:3)
# mip <- inspect_mip(data, rules)
# mip <- miprules(rules)
# mip
# le <- log_extract(mip$._vars_num)
# le
# log_enrich_data(data, le)
# log_enrich_data(list(x=1), le)
# rules <- validator( x > 10, log(x)  < log(8))
# rules
#
# d <- data.frame(x = 9)
# mip <- miprules(rules)
# log_values <- log_derived_data(values, mip$._log_transform)
#
# mip$set_values( values
#                 , log_values = log_values
#                 , delta_names = mip$._log_transform$num_vars
# )
#
# mip$._value_rules <- c(mip$._value_rules, log_constraint_rules("x", "x._log"))
# mip$._value_rules
# mip$execute()
