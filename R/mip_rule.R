#' Create a rule used by mip
#'
#' Create a rule used by mip
#' @param a named vector with coefficients
#' @param op operator in ("<=", "==", ">=", ">", "<")
#' @keywords internal
mip_rule <- function(a, op, b, rule, type, weight=Inf, ...){
  if (missing(type)){
    type <- rep("double", length(a))
    names(type) <- names(a)
  }
  structure( list( a=a, op=op, b=unname(b)
                 , rule   = rule
                 , type   = type
                 , weight = weight
                 )
           , class="mip_rule"
           )
}

as.character.mip_rule <- function(x, ...){
  a <- paste0(x$a, "*", names(x$a), collapse= ' + ')

  # do some simplication
  a <- gsub("\\b1\\*", "", a) # "1*" => ""
  a <- gsub("\\+ -", "- ", a) # "+ -" => "- "

  paste0(a, " ",x$op, " ", x$b, sep = "")
}

print.mip_rule <- function(x, ...){
  a <- paste0(x$a, "*", names(x$a), collapse= ' + ')

  # do some simplication
  a <- gsub("\\b1\\*", "", a) # "1*" => ""
  a <- gsub("\\+ -", "- ", a) # "+ -" => "- "

  cat(x$rule, ": ", a, " ",x$op, " ", x$b, sep = "")
}

rewrite_mip_rule <- function(x, ...){
  if (x$op == '>='){
    x$a <- -x$a
    x$op <- '<='
    x$b <- -x$b
  } else if (x$op == ">"){
    x$a <- -x$a
    x$op <- '<'
    x$b <- -x$b
  }
  x
}

# get variables from a list of mip_rule objects
get_mr_vars <- function(x, ...){
  unique(unlist(lapply(x, function(r) names(r$a))))
}

# get rules names from a list of mip_rule objects
get_mr_rules <- function(x, ...){
  sapply(x, function(r){r$rule})
}

# get a coefficient matrix from a list of mip_rule objects
get_mr_matrix <- function(x, ...){
  variable <- get_mr_vars(x, ...)
  rule <- get_mr_rules(x, ...)
  n_rule <- length(rule)
  n_variable <- length(variable)

  A <- matrix( 0
             , nrow=n_rule, ncol=n_variable
             , dimnames = list(rule=rule, variable=variable)
             )

  for (i in seq_len(n_rule)){
    a <- x[[i]]$a
    A[i, names(a)] <- a
  }
  op <- sapply(x, `[[`, 'op')
  b <- unname(sapply(x, `[[`, 'b'))

  list(A=A, operator=op, b=b)
}

get_mr_type <- function(x, ...){
  type <- unlist(sapply(x, function(mr){
    mr$type
  }, simplify = FALSE))
  vars <- names(type)
  df <- unique(data.frame(vars=vars, type=type, stringsAsFactors = FALSE))
  setNames(df$type, df$vars)
}

get_mr_expression <- function(x, ...){
  expr <- parse(text=sapply(x, as.character))
  names(expr) <- get_mr_rules(x, ...)
  expr
}

get_mr_weights <- function(x, ...){
  weight <- sapply(x, function(r){r$weight})
  names(weight) <- get_mr_rules(x)
  weight
}
