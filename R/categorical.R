# determine if a rule is categorical
is_cat <- function(expr, or=TRUE, ...){
  # this allows for logicals such as "if (A) B"
  if (is.symbol(expr)){
    return(TRUE)
  }

  if(is.atomic(expr)){
    return(is.logical(expr))
  }

  op = op_to_s(expr)
  l <- left(expr)
  r <- right(expr)

  conj1 <- if (or) "|" else "&"
  conj2 <- if (or) "||" else "&&"

  switch (op,
    "%in%" = TRUE,  # allow all literals (should check for character and logical)
    "("    = is_cat(l, or),
    "!"    = is_cat(l, !or),
    "=="   = is.character(r) || is.logical(r),
    "!="   = is.character(r) || is.logical(r),
    "if"   = is_cat(l, !or) && is_cat(r, or),
    conj1  = is_cat(l, or) && is_cat(r, or),
    conj2  = is_cat(l, or) && is_cat(r, or),
    FALSE
  )
}

# cat var info, utility function for collecting info with get_catvar
cvi <- function(var, value, not){
  list(list(
    var = deparse(var),
    value = eval(value), # we might want to evaluate in higher frame!
    not = not)) # this indicates if "var %in% value" or "!(var %in% value)"
}

# collect variable information within a rule, assumes that is_cat has been used to check wether
# it is categorical
get_catvar <- function(expr, not = FALSE){
  if (is.symbol(expr)){
    return(cvi(expr, TRUE, not))
  }

  op = op_to_s(expr)
  l <- left(expr)
  r <- right(expr)

  switch (op,
          "%in%" = cvi(l, r, not),
          "=="   = cvi(l, r, not),
          "!="   = cvi(l, r, !not),
          "if"   = c( get_catvar(l, !not), get_catvar(r, not)),
          "("    = get_catvar(l, not),
          "!"    = get_catvar(l, !not),
          "|"    = c( get_catvar(l, not), get_catvar(r, not)),
          "||"   = c( get_catvar(l, not), get_catvar(r, not)),
          "&"    = c( get_catvar(l, not), get_catvar(r, not)),
          "&&"   = c( get_catvar(l, not), get_catvar(r, not)),
          NULL
  )
}

# generate binary variable names from vars and there values.
bin_var_name <- function(x, infix=":"){
  if (is.character(x$value)){
    paste0(x$var, infix, x$value)
  } else {
    x$var
  }
}

#' Check if rules are categorical
#'
#' Check if rules are categorical
#' @export
#' @param x validator object
#' @return logical indicating which rules are purely categorical/logical
is_categorical <- function(x, ...){
  sapply(x$rules, function(rule){
    is_cat(rule@expr)
  })
}

#' Get coefficient matrix from categorical edits
#'
#' Get coefficient matrix from categorical edits, similar to
#' linear_coefficients.
#'
#' TODO explain mapping to coefficients
#' @param x validator object
#' @export
cat_coefficients <- function(x, ...){
  stopifnot(inherits(x, "expressionset"))
  cat_rules <- x[is_categorical(x)]
  mr <- lapply(cat_rules$rules, cat_coef)
  get_mr_matrix(mr)
}

cat_coef <- function(rule, ...){
  rule_l <- get_catvar(rule@expr)
  a <- unlist(lapply(rule_l, function(x){
    vars <- bin_var_name(x)
    coef <- rep(if(x$not) -1L else 1L, length(vars))
    names(coef) <- vars
    coef
  })
  )
  b <- 1 - sum(sapply(rule_l, function(x){
    x$not
  }))
  op = ">="
  mip_rule(a, op, b, rule@name)
}


# test
# e <- expression(
#   if (A) B,
#   A | B,
#   !(A)|B,
#   x > 1
# )
#
#sapply(e, is_conditional)
# rules <- validator( x>1, if (y>2) x>1, a %in% c("A1", "A2"), if (a %in% c("A1","A2")) b %in% "B", if (c==TRUE) d==TRUE)
# is_categorical(rules)
# cat_rules <- rules[is_categorical(rules)]
# cvs <- get_catvars(cat_rules)
# get_binary_vars(cvs)
# cat_coef(cat_rules[[2]])
# cat_coefficients(cat_rules)
