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
#' @param ... not used
expect_values <- function(values, weights, ...){
  if (missing(weights)){
    weights <- rep(1, length(values))
    names(weights) <- names(values)
  }

  stopifnot(
    length(values) == length(weights),
    all(names(values) %in% names(weights))
  )
  # assure that weights have same order as values
  #weights <- weights[names(values)]

  is_numeric <- vapply(values, is.numeric, TRUE)
  lin_values <- values[is_numeric]
  lin_is_na <- vapply(lin_values, is.na, TRUE)

  lin_values[lin_is_na] <- -1
  lin_rules1 <- lapply(names(lin_values), function(n){
    a <- setNames(1, n)
    b <- lin_values[[n]]
    w <- weights[n]
    if (is.finite(w)){
      soft_lin_rule(mip_rule(a, "<=", b, n, w))
    } else {
      mip_rule(a, "==", b, n, Inf)
    }
  })

  # set all NA values to 1 to create contradictory statement
  lin_values[lin_is_na] <- 1

  lin_rules2 <- lapply(names(lin_values), function(n){
    a <- setNames(1, n)
    b <- lin_values[[n]]
    w <- weights[n]
    if (is.finite(w)){
      soft_lin_rule(mip_rule(-a, "<=", -b, n, w))
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
      names(a) <- n
      if (!value){
        a <- -a
        b <- 0
      }
    }
    soft_cat_rule(mip_rule(a, "==", b, n, weights[n], type=sapply(a, function(x) "binary")))
  })

  c(lin_rules1, lin_rules2, cat_rules)
}
