# code is mainly copied from validate, but needed for linear sub expressions in
# conditional statements.

#' Check which rules are linear rules.
#' @export
#' @param x \code{\link{validator}} object containing data validation rules
#' @param ... not used
#' @return \code{logical} indicating which rules are (purely) linear.
is_linear <- function(x, ...){
  stopifnot(inherits(x, "validator"))
  sapply(x$rules, function(rule){
    is_lin_(rule@expr)
  })
}

# HACK
lin_as_mip_rules <- function(x, ...){
  lin_rules <- x[is_linear(x)]

  lapply(lin_rules$rules, function(rule){
    rewrite_mip_rule(lin_mip_rule_(rule@expr, name=rule@name), eps=0)
  })
}

# check if a (sub) expression is linear
is_lin_ <- function(expr, top=TRUE, ...){

  op <- op_to_s(expr)
  l <- left(expr)
  r <- right(expr)

  if (top){
    if (!(op %in% c("==", ">", ">=", "<=", "<"))){ return(FALSE) }
    return(is_lin_(l, FALSE) && is_lin_(r, FALSE))
  }

  if (is.atomic(expr)){
    return(is.numeric(expr) || is.null(expr))
  }

  if (is.symbol(expr)){ return(TRUE) }

  if (op %in% c("+","-")){
      return( is_lin_(l, FALSE) && is_lin_(r, FALSE))
    }

  if (op == "*"){
      if (is.numeric(l) || is.numeric(left(l))){ return(is_lin_(r, FALSE)) }
      if (is.numeric(r) || is.numeric(left(r))){ return(is_lin_(l, FALSE)) }
  }
  FALSE
}
#
# create a linear mip_rule from a linear expression.
# assumes that it is checked with is_lin_
lin_mip_rule_ <- function(e, sign=1, name, ...){

  if (is.symbol(e)){
    return(setNames(sign, deparse(e)))
  }

  if (is.numeric(e)){
    return(c(.b=sign*e))
  }

  if (is.null(e)){  # catches unary operators +-
    return(NULL)
  }

  op <- op_to_s(e)
  l <- left(e)
  r <- right(e)

  if (op %in% c("==", ">", ">=", "<=", "<")){
    coef <- c(lin_mip_rule_(l, sign), lin_mip_rule_(r, -sign), .b=0) # makes sure that .b exists
    coef <- tapply(coef, names(coef), sum) # sum up coefficients
    b <- names(coef) == ".b"
    return(mip_rule(coef[!b], op, -coef[b], name))
  }

  if (op == '-'){
    if (is.null(r)){ # unary "-l"
      return(lin_mip_rule_(l, -sign))
    } # else binary "l-r"
    return(c(lin_mip_rule_(l, sign), lin_mip_rule_(r, -sign)))
  }

  if (op == '+'){
    if (is.null(r)){ # unary "+l"
      return(lin_mip_rule_(l, sign))
    } # else binary "l+r"
    return(c(lin_mip_rule_(l, sign), lin_mip_rule_(r, sign)))
  }

  if (op == '*'){
    if (is.numeric(left(l))){
      l <- eval(l) # to deal with negative coefficients
    }
    if (is.numeric(l)){ return(lin_mip_rule_(r, sign*l)) }
    
    if (is.numeric(left(r))){
      r <- eval(r) # to deal with negative coefficients
    }
    if (is.numeric(r)){ return(lin_mip_rule_(l, sign*r)) }
  }
  stop("Invalid linear statement")
}

