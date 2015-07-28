#' @param a named vector with coefficients
#' @param op operator in ("<=", "==", ">=", ">", "<")
mip_rule <- function(a, op, b, rule, ...){
  structure( list(a=a, op=op, b=b, rule=rule)
           , class="mip_rule")
}


print.mip_rule <- function(x, ...){
  cat(paste0(x$a, "*", names(x$a), collapse= ' + '), x$op, x$b)
}

get_mr_vars <- function(x, ...){
  unique(unlist(lapply(x, function(r) names(r$a))))
}

get_mr_rules <- function(x, ...){
  sapply(x, function(r){r$rule})
}

get_mr_matrix <- function(x, ...){
  variable <- get_mr_vars(x, ...)
  rule <- get_mr_rules(x, ...)
  n_rule <- length(rule)
  n_variable <- length(variable)

  A <- matrix(0, nrow=n_rule, ncol=n_variable, dimnames = list(rule=rule, variable=variable))
  for (i in seq_len(n_rule)){
    a <- x[[i]]$a
    A[i, names(a)] <- a
  }
  op <- sapply(x, `[[`, 'op')
  b <- sapply(x, `[[`, 'b')

  list(A=A, operator=op, b=b)
}
