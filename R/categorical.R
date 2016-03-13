INFIX_CAT_NAME <- ":"

# TODO maybe change the code below to directly generate mip_rules
# determine if a rule is categorical
is_cat_ <- function(expr, or=TRUE, ...){
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

  switch (op,
    "%in%" = TRUE,  # allow all literals (should check for character and logical)
    "("    = is_cat_(l, or),
    "!"    = is_cat_(l, !or),
    "=="   = is.character(r) || is.logical(r),
    "!="   = is.character(r) || is.logical(r),
    "if"   = is_cat_(l, !or) && is_cat_(r, or),
    "|"    = or && is_cat_(l, or) && is_cat_(r, or),
    "||"   = or && is_cat_(l, or) && is_cat_(r, or),
    "&"    = !or && is_cat_(l, or) && is_cat_(r, or),
    "&&"   = !or && is_cat_(l, or) && is_cat_(r, or),
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

# collect variable information within a rule, assumes that is_cat_ has been used
# to check wether it is categorical
get_catvar <- function(expr, not = FALSE){
  if (is.symbol(expr)){
    return(cvi(expr, TRUE, not))
  }

  op = op_to_s(expr)
  l <- left(expr)
  r <- right(expr)

  switch ( op,
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

# generate binary variable names from vars and their values.
bin_var_name <- function(x, infix=INFIX_CAT_NAME){
  if (is.character(x$value)){
    paste0(x$var, infix, x$value)
  } else {
    x$var
  }
}

# input is mip_rule, results is character vector with infix names
cat_var_name <- function(x, infix=INFIX_CAT_NAME){
  suffix <- paste0(infix, ".*$")
  gsub(suffix,"",names(x$a))
}

#' Check if rules are categorical
#'
#' Check if rules are categorical
#' @export
#' @param x validator object
#' @param ... not used
#' @return logical indicating which rules are purely categorical/logical
#' @example examples/categorical.R
is_categorical <- function(x, ...){
  sapply(x$rules, function(rule){
    is_cat_(rule@expr)
  })
}

#' Get coefficient matrix from categorical rules
#'
#' Get coefficient matrix from categorical edits, similar to
#' linear_coefficients.
#'
#' @param x validator object
#' @param ... not used
#' @keywords internal
cat_coefficients <- function(x, ...){
  stopifnot(inherits(x, "expressionset"))
  mr <- cat_as_mip_rules(x, ...)
  get_mr_matrix(mr)
}

#' get categorical rules as mip_rules
#'
#' @param x expressionset object
#' @param ... not used
#' @return list of mip_rule
#' @keywords internal
cat_as_mip_rules <- function(x, ...){
  cat_rules <- x[is_categorical(x)]
  lapply(cat_rules$rules, function(rule){
    cat_mip_rule_(rule@expr, name=rule@name)
  })
}

cat_mip_rule_ <- function(e, name, ...){
  rule_l <- get_catvar(e)
  a <- unlist(lapply(rule_l, function(x){
    vars <- bin_var_name(x)
    # if (x %in% set) +1, if (!(x %in% set)) -1
    coef <- rep(if(x$not) -1L else 1L, length(vars))
    names(coef) <- vars
    coef
  })
  )

  # sum(a_pos) + sum(1-a_neg) >= 1
  # condition is that at least one of the variable is true, extract the negated memberships
  b <- 1 - sum(sapply(rule_l, function(x){
    x$not
  }))

  if ( length(rule_l) == 1){
    if (length(a) > 1 || op(e) == "=="){  # this is a strict(er) version and allows for some optimization
      mip_rule(a, "==", b, name, type=sapply(a, function(x) 'binary'))
    } else {
      mip_rule(a, "<=", b, name, type=sapply(a, function(x) 'binary')) # needed for logical variables
    }
  } else {
    mip_rule(-a, "<=", -b, name, type=sapply(a, function(x) 'binary')) # normalized version of a*x >= b
  }
}
