
# convert statements of A == '1' into A + .delta_A == '1'
soft_cat_rule <- function(x, prefix=".delta_", name = x$rule,  ...){
  #stopifnot(length(x$a) == 1 && x$op == '==')
  delta <- setNames(1L, paste0(prefix, name, collapse = ""))
  x$a <- c(x$a, delta)
  x
}

soft_lin_rule <- function( x, prefix=".delta_", name = x$rule, ...
                         , M = 1e7
                         ){
  # for the moment assume a mip_rule abides a*x <= b
  # so a soft rules is of form a*x <= b + M*delta
  delta <- setNames(-M, paste0(prefix, name, collapse = ""))
  x$a <- c(x$a, delta)
  x
}

#' @param x named list of values
#' @param weights named numeric of equal length as values
expect_values <- function(values, weights, ...){
  if (missing(weights)){
    weights <- rep(1, length(values))
    names(weights) <- names(values)
  }

  stopifnot(
    length(values) == length(weights),
    all(names(values) %in% names(weights))
  )

  is_numeric <- sapply(values, is.numeric)

  lin_values <- values[is_numeric]
  cat_values <- values[!is_numeric]

  lin_rules <- lapply(names(lin_values), function(n){
    a <- setNames(1, n)
    b <- lin_values[[n]]
    soft_lin_rule(mip_rule(a, "==", b, weights[n]))
  })

  cat_values <- values[!is_numeric]

  lin_rules <- lapply(names(lin_values), function(n){
    a <- setNames(1, n)
    b <- lin_values[[n]]
    soft_lin_rule(mip_rule(a, "==", b, n, weights[n]))
  })

  cat_rules <- lapply(names(cat_values), function(n){
    nm <- paste0(n, INFIX_CAT_NAME, lin_values[[n]])
    a <- setNames(1, nm)
    soft_cat_rule(mip_rule(a, "==", 1, n, weights[n]))
  })

  append(lin_rules, cat_rules)
}

# replace equalities with
replace_equal_mip_rules <- function(x, ...){
  unlist(
    lapply(x, function(mr){
    if (mr$op == "=="){
      mr$op <- "<="
      list(mr, mip_rule(-mr$a, "<=", -mr$b, mr$rule, mr$weight))
    } else {
      list(mr)
    }
    }), recursive = FALSE
  )
}



### testing
# a <- c("A:a1"=1)
# mr <- mip_rule(a, "==", 1, "A")
# soft_cat_rule(mr)
#
# x <- c(x=1)
# mr <- mip_rule(x, "<=", 2, "x")
# soft_lin_rule(mr)
#
#
# v1 <- validator(x == 1, y+1==z)
# x <- lin_as_mip_rules(v1)
# replace_equal_mip_rules(x)
#
# expect_values(list(x=1, a="A"))
