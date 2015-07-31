# code is mainly copied from validate, but needed for linear sub expressions in
# conditional statements.

is_linear <- function(x, ...){
  stopifnot(inherits(x, "validator"))
  x$is_linear()
}

linear_coefficients <- function(x, ...){
  stopifnot(inherits(x, "validator"))
  x$linear_coefficients()
}

# HACK
lin_as_mip_rules <- function(x, ...){
  lc <- x$linear_coefficients()
  rule_names <- row.names(lc$A)
  lapply(seq_along(lc$operators), function(i){
    a <- lc$A[i,]
    mip_rule( a[a!=0]
            , lc$op[i]
            , lc$b[i]
            , rule_names[i]
            )
  })
}

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
      if (is.numeric(l)){ return(is_lin_(r, FALSE)) }
      if (is.numeric(r)){ return(is_lin_(l, FALSE)) }
  }
  FALSE
}
#
#
get_num_var <- function(e, sign=1, ...){

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
    coef <- c(get_num_var(l, sign), get_num_var(r, -sign), .b=0) # makes sure that .b exists
    coef <- tapply(coef, names(coef), sum)
    b <- names(coef) == ".b"
    return(mip_rule(coef[!b], op, coef[b], ""))
  }

  if (op == '-'){
    if (is.null(r)){ return(get_num_var(l, -sign))}
    return(c(get_num_var(l, sign), get_num_var(r, -sign)))
  }

  if (op == '+'){
    return(c(get_num_var(l, sign), get_num_var(r, sign)))
  }

  if (op == '*'){
    if (is.numeric(l)){ return(get_num_var(r, sign*l))}
    if (is.numeric(r)){return(get_num_var(l, sign*r))}
  }
  stop("Invalid linear statement")
}
#
# is_linear <- function(expr, ...){
#   stopifnot(is.expression(expr))
#   sapply(expr, is_lin)
# }
#
# testing
# e <- expression(x>1, a==2, a=="b", a>b, z==a*b, a==2+a-x, x==-2)
# is_linear(e)

e <- quote(x + 2*y > z + 3)
#get_num_var(e)

