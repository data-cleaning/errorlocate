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
    "%in%" = is.character(r),
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
  var <- deparse(var)
  list(list(var = var, value = value, not = not))
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

# utility function for retrieving all variable information of all rules
get_catvars <- function(rules, ...){
  cvs <- lapply(rules$rules, function(rule){
    get_catvar(rule@expr)
  })
  setNames(cvs, names(rules))
#   vars <- sapply(csv, `[[`, "var")
#   tapply(csv, vars, c)
}

# utility function for retrieving all binary variables, needed for mip
get_binary_vars <- function(cvs, infix=":"){
  var_list <- unlist(csv, recursive = FALSE, use.names = FALSE)
  bin_vars <- sapply(var_list, function(x){
    if (is.character(x$value)){
      paste0(x$var, infix, x$value)
    } else {
      x$var
    }
  })
  unique(bin_vars)
}

#' @export
#' @param rules validator object
#' @return logical indicating which rules are purely categorical/logical
is_categorical <- function(rules, ...){
  sapply(rules$rules, function(rule){
    is_cat(rule@expr)
  })
}

# test
# e <- expression(
#   if (A) B,
#   A | B,
#   !(A)|B,
#   x > 1
# )
#
# sapply(e, is_conditional)
# rules <- validator( x>1, if (y>2) x>1, a %in% c("A1", "A2"), if (a %in% "A") b %in% "B", if (c==TRUE) d==TRUE)
# is_categorical(rules)
# cat_rules <- rules[is_categorical(rules)]
# csv <- get_catvars(cat_rules)
# get_binary_vars(csv)
