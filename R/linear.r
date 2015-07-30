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

# is_lin <- function(expr, top=TRUE, ...){
#   if (!top){
#     if (is.atomic(expr)){
#       return(is.numeric(expr))
#     }
#
#     if (is.symbol(expr)){
#       return(TRUE)
#     }
#   }
#
#   op <- op_to_s(expr)
#   l <- left(expr)
#   r <- right(expr)
#
#   if (top){
#     if (op %in% c("==", ">", ">=", "<=", "<")){
#       is_lin(l, FALSE) && is_lin(r, FALSE)
#     } else {
#       FALSE
#     }
#   } else {
#     if (length(expr) == 2 && op %in% c("-", "+")){
#       is.numeric(l)
#     } else if (op %in% c("+","-")){
#       is_lin(l, FALSE) && is_lin(r, FALSE)
#     } else if (op == "*"){
#       (is.numeric(l) || is.numeric(r)) && is_lin(l, FALSE) && is_lin(r, FALSE)
#     } else{
#       FALSE
#     }
#   }
# }
#
#
# get_num_var <- function(e, sign=1, ...){
#
#   if (is.symbol(e)){
#     return(setNames(sign, deparse(e)))
#   }
#
#   if (is.numeric(e)){
#     return(c(.CONSTANT=e))
#   }
#
#   op <- op_to_s(e)
#   l <- left(e)
#   r <- right(e)
#
#   if (op %in% c("==", ">", ">=", "<=", "<")){
#     return(c(get_num_var(l, sign), get_num_var(r, -sign)))
#   }
#
#   if (length(e) == 2){
#   }
# }
#
# is_linear <- function(expr, ...){
#   stopifnot(is.expression(expr))
#   sapply(expr, is_lin)
# }
#
# testing
# e <- expression(x>1, a==2, a=="b", a>b, z==a*b, a==2+a-x, x==-2)
# is_linear(e)

