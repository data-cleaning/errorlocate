#' Create a rule used by mip
#'
#' Create a rule used by mip
#' @param a named vector with coefficients
#' @param op operator in ("<=", "==", ">=", ">", "<")
#' @keywords internal
mip_rule <- function(a, op, b, rule, weight=Inf, ...){
  structure( list(a=a, op=op, b=b, rule=rule, weight=Inf)
           , class="mip_rule")
}


print.mip_rule <- function(x, ...){
  cat(x$rule, ": ", paste0(x$a, "*", names(x$a), collapse= ' + '), x$op, x$b, sep = "")
}

rewrite_mip_rule <- function(x, eps=1e-5, ...){
  switch(x$op,
    ">=" = list(a = -x$a, op="<=", b=-x$b),
    ">"  = list(a = -x$a, op="<=", b=-x$b - eps), # subtract epsilon to simulate strict inequality
    "<"  = list(a =  x$a, op="<=", b= x$b - eps),
    x
  )
}

# get variables from a list of mip_rule objects
get_mr_vars <- function(x, ...){
  unique(unlist(lapply(x, function(r) names(r$a))))
}

# get rules names from a list of mip_rule objects
get_mr_rules <- function(x, ...){
  sapply(x, function(r){r$rule})
}

# get a coefficient matrix from a list of mip_rule objects
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

get_mr_weights <- function(x, ...){
  weight <- sapply(x, function(r){r$weight})
  setNames(weight, get_mr_rules(x))
}
