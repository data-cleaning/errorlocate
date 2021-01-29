#TODO rename to mip_*

# convert statements of A == '1' into A + .delta_A == '1'
soft_cat_rule <- function(x, prefix=".delta_", name = x$rule,  ...){
  stopifnot(inherits(x, "mip_rule"))
  nm <- paste0(prefix, name, collapse = "")
  delta <- setNames(1L, nm)
  x$a <- c(x$a, delta)
  x$type <- c(x$type, setNames("binary", nm))
  x
}

soft_lin_rule <- function( x, prefix=".delta_", name = x$rule, ...
                         , M = 1e7
                         ){
  # assumes a mip_rule abides a*x <= b
  # so a soft rules is of form a*x <= b + M*delta
  stopifnot(inherits(x, "mip_rule"))
  nms <- c(names(x$a), paste0(prefix, name, collapse = ""))
  x$a <- c(x$a, -M)
  x$type <- c(x$type, "binary")
  names(x$a) <- nms
  names(x$type) <- nms
  x
}

suffix <- function(suffix){
  function(x){
    paste0(x, suffix)
  }
}

eps_plus <- suffix("_eps_plus")
eps_min <- suffix("_eps_min")

#' expect values
#'
#' @param values named list of values.
#' @param weights named numeric of equal length as values.
#' @param delta_names alternative names for binary variables. (used for log)
#' @param ... not used
#' @keywords internal
expect_values <- function(values, weights, delta_names = NULL, ...){
  if (missing(weights)){
    weights <- rep(1, length(values))
    names(weights) <- names(values)
  }

  if (is.null(delta_names)){
    delta_names <- names(values)
    names(delta_names) <- delta_names
  }

  stopifnot(
    length(values) == length(weights),
    all(names(values) %in% names(weights))
  )
  # assure that weights have same order as values
  #weights <- weights[names(values)]

  is_numeric <- vapply(values, is.numeric, TRUE)
  lin_values <- values[is_numeric]

  # otherwise the problem become unstable...
  is.na(lin_values) <- lin_values >= 1e7

  lin_is_na <- vapply(lin_values, is.na, TRUE)

  lin_values <- lin_values[!lin_is_na]

  lin_rules1 <- lapply(names(lin_values), function(n){
    a <- setNames(1, n)
    b <- lin_values[[n]]
    w <- weights[n]
    if (is.finite(w)){
      n_ub <- paste0(n, "_ub")
      soft_lin_rule( mip_rule(a, op="<=", b =  b, rule = n_ub, weight = w)
                   , name = delta_names[n]
                   )
    } else {
      mip_rule(a, op = "==", b = b, rule = n, weight = Inf)
    }
  })

  lin_rules2 <- lapply(names(lin_values), function(n){
    a <- setNames(1, n)
    b <- lin_values[[n]]
    w <- weights[n]
    if (is.finite(w)){
      n_lb <- paste0(n, "_lb")
      soft_lin_rule( mip_rule(-a, op = "<=", b = -b, rule = n_lb, weight = w)
                   , name = delta_names[n]
                   )
    } else {
      NULL
    }
  })

  cat_values <- values[!is_numeric]
  cat_rules <- lapply(names(cat_values), function(n){
    value <- cat_values[[n]]
    a <- setNames(1, paste0(n, INFIX_CAT_NAME, value))
    b <- 1

    if (is.logical(value)){
      # rewrite (only need one column)
      names(a) <- n
      if (!isTRUE(value)){
        a <- -a
        b <- 0
      }
      # if NA, just skip this constraint, should this also hold for categories?
      if (is.na(value)){
        a[] <- 0 # effectively set delta to 1
        b <- 1
      }
    }
    soft_cat_rule(mip_rule(a, op = "==", b = b, rule = n, weight = weights[n], type=sapply(a, function(x) "binary")))
  })

  c(lin_rules1, lin_rules2, cat_rules)
}
