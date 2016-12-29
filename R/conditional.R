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
    "|"   = or  && is_condition_(l, or, FALSE) && is_condition_(r, or, FALSE),
    "||"  = or  && is_condition_(l, or, FALSE) && is_condition_(r, or, FALSE),
    "&"   = !or && is_condition_(l, or, FALSE) && is_condition_(r, or, FALSE),
    "&&"  = !or && is_condition_(l, or, FALSE) && is_condition_(r, or, FALSE),
    "!"   = is_condition_(l, !or, FALSE),
    "("   = is_condition_(l, or, FALSE),
    FALSE
  )
}

#' Check if rules are conditional rules
#'
#' Check if rules are conditional rules
#' @export
#' @param rules validator object containing validation rules
#' @param ... not used
#' @return logical indicating which rules are conditional
#' @example examples/conditional.R
is_conditional <- function(rules, ...){
  stopifnot(inherits(rules, "validator"))
  sapply(rules$rules, function(rule){
    is_condition_(rule@expr)
  })
}

cond_as_mip_rules <- function(x, ...){
  cond_rules <- x[is_conditional(x)]
  mr <- lapply(cond_rules$rules, function(rule){
    #browser()
    prefix <- paste0(rule@name, "._lin")

    rl <- replace_linear(rule@expr, prefix=prefix)
    mr_cat <- cat_mip_rule_(rl$cat, rule@name)

    # convert linear expressions to linear mip_rules
    mr_lin <- mapply( lin_mip_rule_, rl$linear, name=names(rl$linear),
                      SIMPLIFY = FALSE, USE.NAMES = FALSE
                    )
    # normalize them (">", ">=" into "<", "<=")
    mr_lin <- lapply(mr_lin, rewrite_mip_rule)
    # make them soft/conditional on the variable <v>._<count> used in mr_cat
    mr_lin <- lapply(mr_lin, soft_lin_rule, prefix="")
    append(list(mr_cat), mr_lin)
  })
  unlist(mr, recursive = FALSE)
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
  #browser()
  op <- op_to_s(e)
  l <- left(e)
  r <- right(e)

  if (is.atomic(e) || is.symbol(e)){
    return(e)
  }

  if (is_lin_(e)){
    if (!or){
      e <- invert_(e)
    }
    h$expr <- append(h$expr, e)
    prefix <- if (is.null(h$prefix)) ".v" else h$prefix

    name <- paste0(prefix, length(h$expr))
    names(h$expr)[length(h$expr)] <- name

    if (or){
      return(substitute(!name, list(name=as.symbol(name))))
    } else {
      return(substitute(name, list(name=as.symbol(name))))
    }
  }

  switch (op,
    "if" = substitute( if (l) r,
                       list( l=rep_lin_(l, !or, h), r=rep_lin_(r, or, h))),
    "|"  = substitute( l | r,
                       list( l=rep_lin_(l, or, h), r=rep_lin_(r, or, h))),
    "&&" = substitute( l && r,
                       list( l=rep_lin_(l, or, h), r=rep_lin_(r, or, h))),
    "&"  = substitute( l & r,
                       list( l=rep_lin_(l, or, h), r=rep_lin_(r, or, h))),
    "||" = substitute( l || r,
                       list( l=rep_lin_(l, or, h), r=rep_lin_(r, or, h))),
    "!"  = substitute( !l,
                       list(l=rep_lin_(l, !or, h))),
    "("  = substitute( (l),
                       list(l=rep_lin_(l, or, h))),
    e
  )
}
