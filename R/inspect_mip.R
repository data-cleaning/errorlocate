#' inspect the mip problem formulation
#'
#' Utility function to inspect the mip problem for a record. `inspect_mip` can
#' be used as a "drop-in" replacement for [locate_errors()], but works on the
#' first record.
#'
#' It may sometimes be handy to find out what is happening exactly with a record.
#' See the example section for finding out what to do with inspect_mip. See
#' `vignette("inspect_mip")` for more details.
#' @example ./examples/inspect_mip.R
#' @family Mixed Integer Problem
#' @export
#' @inheritParams locate_errors
inspect_mip <- function(data, x, weight, ...){
  rules <- x
  stopifnot(inherits(rules, "validator"))
  w <- sapply(data, function(n) 1)
  if (!missing(weight)){
    if (!is.null(names(weight))){
      w[names(weight)] <- weight
    } else if (length(weight) == length(w)){
      w[] <- weight
    } else {
      stop("invalid weight specification")
    }
  }
  weight <- w

  if (is.data.frame(data) && nrow(data) > 1){
    warning("Taking record 1, ignoring rest of the records..."
           , call. = FALSE)
    data <- data[1,,drop=FALSE]
  }
  data <- as.list(data)

  mip <- miprules(rules)

  vars <- mip$._vars
  missing_vars <- vars[!vars %in% names(data)]
  if (length(missing_vars)){
    # if they are part of the environment remove...
    mv_in_env <- sapply(missing_vars, exists)
    vars <- setdiff(vars, missing_vars[mv_in_env])
    mip$._vars <- vars
  }

  mip$set_values(data, weight)
  mip$update_log_constraints(data, ...)
  mip
}


# rules <- validator(x > 1)
# data <- list(x = 0)
#
# inspect_mip(data, rules)
# mip$rules
# mip$mip_rules()
# mip$objective
# mip$to_lp()
#
# res <- mip$execute()
# res
# res$lp
