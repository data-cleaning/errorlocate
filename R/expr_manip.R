negate_ <- function(e, ...){
  # don't do double negation: that complicates analysis of expressions
  op <- node(e)

  if (op == '!'){
    return(consume(e[[2]]))
  }

  expr <- if (is.call(e) && op != '('){
    if (op == "!="){
      substitute( l == r, list(l = left(e), r = right(e)))
    } else if (op == "=="){
      substitute( l != r, list(l = left(e), r = right(e)))
    }
    else {
      substitute( !(e), list(e=e) )
    }
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
