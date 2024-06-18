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

#' @export
as.character.mip_rule <- function(x, ...){
  a <- paste0(x$a, "*", names(x$a), collapse= ' + ')

  # do some simplication
  a <- gsub("\\b1\\*", "", a) # "1*" => ""
  a <- gsub("\\+ -", "- ", a) # "+ -" => "- "

#  browser()
  paste0(a, " ",x$op, " ", x$b, sep = "")
}

print.mip_rule <- function(x, ...){
  a <- paste0(x$a, "*", names(x$a), collapse= ' + ')

  # do some simplification
  a <- gsub("(?<=[^\\d.]|^)1\\*", "", a, perl = T) # "1*" => ""
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
  # needed for variables sticking together
  # could need some
  variable <- sort(variable)
  # just aesthetics...
  .delta <- grepl("^.delta|._lin", variable)
  variable <- c(variable[!.delta], variable[.delta])
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

get_mr_l_constraint <- function(x, ..., eps_strict = 1e-3){
  variable <- get_mr_vars(x, ...)
  # needed for variables sticking together
  # could need some
  variable <- sort(variable)
  # just aesthetics...
  .delta <- grepl("^.delta|._lin", variable)
  variable <- c(variable[!.delta], variable[.delta])
  rule <- get_mr_rules(x, ...)
  n_rule <- length(rule)
  n_variable <- length(variable)

  L <- matrix( 0
             , nrow=n_rule, ncol=n_variable
             , dimnames = list(rule=rule, variable=variable)
  )

  for (i in seq_len(n_rule)){
    a <- x[[i]]$a
    L[i, names(a)] <- a
  }
  dir <- sapply(x, `[[`, 'op')
  rhs <- unname(sapply(x, `[[`, 'b'))

  # fix strict inequalities
  lt <- (dir == "<")
  gt <- (dir == ">")

  rhs[lt] <- rhs[lt] - eps_strict
  dir[lt] <- "<="

  rhs[gt] <- rhs[gt] + eps_strict
  dir[gt] <- ">="
  #

  names <- colnames(L)

  lc <- ROI::L_constraint(
    L = L,
    dir = dir,
    rhs = rhs,
    names = names
  )

  # add categorical contraints
  cc <- get_cat_constraints(names)

  # combine the two
  c(lc, cc)
}

get_cat_constraints <- function(vars){
  #TODO also add log lower boundary as SOS
  CAT <- ":.+"
  # CAT <- "(:|\\._).+"

  idx <- grepl(CAT, vars)
  var <- sub(CAT, "", vars)

  sosname <- unique(var[idx])
  N <- length(sosname)

  if (N < 1){
    return(NULL)
  }

  L <- matrix(
    0,
    nrow = N,
    ncol = length(vars),
    dimnames = list(
      rule=paste0(".cat.", sosname),
      var = vars
    )
  )

  for (i in seq_len(N)){
    cv <- sosname[i]
    L[i, which(var == cv)] <- 1
  }

  ROI::L_constraint(
    L = L,
    dir = rep("<=", N),
    rhs = rep(1, N),
    names = vars
  )
}


get_mr_type <- function(x, ...){
  type <- unlist(sapply(x, function(mr){
    mr$type
  }, simplify = FALSE))
  vars <- names(type)
  df <- unique(data.frame(vars=vars, type=type, stringsAsFactors = FALSE))
  setNames(df$type, df$vars)
}

get_mr_numeric_vars <- function(x, ...){
  type <- unlist(sapply(x, function(mr){
    mr$type
  }, simplify = FALSE))
  vars <- unlist(sapply(x, cat_var_name))
  df <- unique(data.frame(vars=vars, type=type, stringsAsFactors = FALSE))
  df$vars[df$type == "double"]
  # setNames(df$type, df$vars)
}

get_mr_expression <- function(x, ...){
  expr <- str2expression(text=sapply(x, as.character))
  names(expr) <- get_mr_rules(x, ...)
  expr
}

get_mr_weights <- function(x, ...){
  weight <- sapply(x, function(r){r$weight})
  names(weight) <- get_mr_rules(x)
  weight
}
