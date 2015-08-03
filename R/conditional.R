is_condition_ <- function(expr, or=TRUE, top=TRUE, ...){
  op <- op_to_s(expr)
  l <- left(expr)
  r <- right(expr)

  if (op == 'if' && !top){
    return(FALSE)
  }

  if (is_lin_(expr) || is_cat_(expr)){
    return(!top) # this prohibits that a pure categorical or linear rule is detected as conditional
  }

  switch (op,
    'if'  = is_condition_(l, !or, FALSE) && is_condition_(r, or, FALSE),
    "|"   = or && is_condition_(l, or, FALSE) && is_condition_(r, or, FALSE),
    "||"  = or && is_condition_(l, or, FALSE) && is_condition_(r, or, FALSE),
    "&"   = !or && is_condition_(l, or, FALSE) && is_condition_(r, or, FALSE),
    "&&"  = !or && is_condition_(l, or, FALSE) && is_condition_(r, or, FALSE),
    FALSE
  )
}

#' Check if rules are conditional rules
#'
#' Check if rules are conditional rules
#'  @export
#' @param rules validator object containing validation rules
#' @return logical indicating which rules are conditional
is_conditional <- function(rules, ...){
  stopifnot(inherits(rules, "validator"))
  sapply(rules$rules, function(rule){
    is_condition_(rule@expr)
  })
}

# replaces linear subexpressions with a binary variable
# assumes that expresssion is conditional
replace_linear <- function(e, prefix=".v"){
  h <- new.env()
  h$prefix <- prefix
  cat <- rep_lin_(e, h=h)

  list( cat    = cat,
        linear = h$expr
      )
}

rep_lin_ <- function(e, or=TRUE, h=new.env()){
  op <- op_to_s(e)
  l <- left(e)
  r <- right(e)

  if (is.atomic(e) || is.symbol(e)){
    return(e)
  }

  if (is_lin_(e)){
    h$expr <- append(h$expr, e)
    prefix <- if (is.null(h$prefix)) ".v" else h$prefix

    name <- paste0(prefix, length(h$expr))
    names(h$expr)[length(h$expr)] <- name

    if (or){
      return(substitute(!name, list(name=as.symbol(name))))
    } else{
      return(substitute(name, list(name=as.symbol(name))))
    }
  }

  switch (op,
    "if" = substitute(if (l) r,
                      list( l=rep_lin_(l, !or, h), r=rep_lin_(r, or, h))),
    "|"  = substitute(l | r,
                      list( l=rep_lin_(l, or, h), r=rep_lin_(r, or, h))),
    "&&" = substitute(l && r,
                     list( l=rep_lin_(l, or, h), r=rep_lin_(r, or, h))),
    "&"  = substitute(l & r,
                      list( l=rep_lin_(l, or, h), r=rep_lin_(r, or, h))),
    "||" = substitute(l || r,
                      list( l=rep_lin_(l, or, h), r=rep_lin_(r, or, h))),
    e
  )
}
