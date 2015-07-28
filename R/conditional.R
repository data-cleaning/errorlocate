is_condition <- function(expr, ...){
  op <- op(expr)
  l <- left(expr)
  r <- right(expr)

  if (op == 'if' || op == '|'){
    is_linear(expr)
  } else {
    FALSE
  }
}

#' @export
#' @param rules validator object
#' @return logical indicating which rules are conditional
is_conditional <- function(rules, ...){
  sapply(rules$rules, function(rule){
    is_condition(rule@expr)
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
# rules <- validator( x>1, if (y>2) x>1)
# is_conditional(rules)
