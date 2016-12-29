# expression manipulation

op <- function(e){
  if (is.call(e)){ e[[1]] }
  else { e }
}

# alias for op
node <- op
op_to_s <- function(e) deparse(op(e))


# short hand when working with if statement
cond <- function(e) e[[2]]
consq <- function(e) e[[3]]

left <- function(e) if (length(e) >= 2) e[[2]]
right <- function(e) if (length(e) >= 3) e[[3]]

negate_ <- function(e, ...){
  # don't do double negation: that complicates analysis of expressions
  op <- node(e)

  if (op == '!'){
    return(e[[2]])
  }

  expr <- if (is.call(e) && op != '('){
    substitute( !(e), list(e=e) )
  } else {
    substitute( !e, list(e=e))
  }
  expr
}

invert_ <- function(e, ...){
  op <- op_to_s(e)
  s <- switch (op,
    "<"   = ">=",
    ">"   = "<=",
    "<="  = ">",
    ">="  = "<",
    #  "==" = "!=",
    # "!="  = "==",
    stop(op, " not supported")
  )
  substitute(a %op% b, list(a=left(e), b=right(e), "%op%"=as.symbol(s)))
}

# checks if an expression contains a literal string
contains_string_ <- function(e, ...){
  if (is.expression(e)){
    return(sapply(e, contains_string_))
  }

  if (is.atomic(e)){
    return(is.character(e))
  }

  if (is.symbol(e)){
    return(FALSE)
  }

  return(  contains_string_(left(e))
        || contains_string_(right(e))
        )
}

contains_value_ <- function(e, value, ...){
  if (is.call(e)){
    op_to_s(e) == value
    contains_value_(left(e), value) || contains_value_(right(e), value)
  } else {
    e == value # return (TRUE | FALSE)
  }
}

collect_ops_ <- function(e){
  if (is.call(e)){
    c(op(e), collect_ops_(left(e)), collect_ops_(right(e)))
  }
}

contains_op_ <- function(e, op, ...){
  if (is.atomic(e) || is.symbol(e)){
    return(FALSE)
  }

  if (op(e) == op){
    return(TRUE)
  }

  contains_op_(left(e), op) || contains_op_(right(e), op)
}

### testing
# e <- quote(x < 1)
# contains_string_(e)
# e <- quote(x == "A")
# contains_string_(e)
# contains_op_(e,"==")
# contains_value_(e, 'A')
#e <- quote(if( (x<1) | z> 3) y> 3)
# e <- quote(x >= 3)
# invert_(e)
