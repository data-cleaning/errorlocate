# makes a copy of the validation object
check_validator <- function(x, copy = TRUE, check_infeasible = TRUE){
  if (!inherits(x, "validator")){
    stop("This method needs a 'validator' object, but was given a '", class(x), "'.",call. = FALSE)
  }
  if (isTRUE(check_infeasible) && is_infeasible(x)){
    stop("This rule set is infeasible. Please fix and repair the rule set with `make_feasible` before continuing.", call. = FALSE)
  }
  invisible(x)
}

to_exprs <- function(x, ..., ratios=TRUE){
  # make names unique
  names(x) <- names(x)

  exprs <-
    x$exprs( lin_eq_eps   = 0
           , lin_ineq_eps = 0
           , replace_in   = FALSE
           , vectorize    = FALSE
           , expand_assignments = TRUE
           , expand_groups = TRUE
           , ...
           )
  if (isTRUE(ratios)){
    exprs[] <- lapply(exprs, rewrite_ratio)
  }
  exprs[] <- lapply(exprs, rewrite_in_range)
  as.expression(exprs)
}

