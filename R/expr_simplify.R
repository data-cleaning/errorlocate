# auxillary functions for simplifying rules

# TRUE | x  -> TRUE
# x | TRUE  -> TRUE
# FALSE | x  -> x
# FALSE | TRUE  -> x
#
# FALSE & x -> FALSE
# x     & FALSE  -> FALSE

# if (TRUE) x  -> x
# if (FALSE) x -> TRUE

simplify_log_expr <- function(expr, ...){
  sle <- simplify_log_expr # shorthand...
  if (length(expr) <= 1){
    return(expr)
  }

  op <- op_to_s(expr)
  l <- left(expr)
  r <- right(expr)

  or <- function(){
    l <- sle(l)
    r <- sle(r)
    if (l == TRUE || r == TRUE){
      TRUE
    } else if (l == FALSE){
      r
    } else if (r == FALSE){
      l
    } else {
      substitute(l | r)
    }
  }

  and <- function(){
    l <- sle(l)
    r <- sle(r)
    if (l == FALSE || r == FALSE){
      FALSE
    } else if (l == TRUE){
      r
    } else if (r == TRUE){
      l
    } else {
      substitute( l & r )
    }
  }

  if_expr <- function(){
    l <- sle(l)
    r <- sle(r)
    if (l == TRUE){
      r
    } else if (l == FALSE){
      TRUE
    } else{
      substitute(if (l) r)
    }
  }

  not <- function(){
    l <- sle(l)
    if (is.logical(l)){
      !l
    } else{
      substitute(!l)
    }
  }


  switch( op,
        "("  = sle(l),
        "|"  = or(),
        "||" = or(),
        "&"  = and(),
        "&&" = and(),
        "if" = if_expr(),
        "!"  = not(),
        expr
      )
}


### testing

# sapply( expression( A | TRUE, A, A | FALSE, if (TRUE) A, if (FALSE) B, if (A|TRUE) B, if (A|FALSE) B, !TRUE, !(A|TRUE), !(A|FALSE))
#       , simplify_log_expr)
