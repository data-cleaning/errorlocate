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

# TODO add to  tests
# rules <- validator(log(x) > log(2))
# data <- data.frame(x = 1:3)
# mip <- miprules(rules)
# mip
# le <- log_extract(mip$._vars_num)
# le
# log_enrich_data(data, le)
# log_enrich_data(list(x=1), le)
