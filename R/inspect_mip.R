#' inspect the mip problem for a record
#'
#' Utility function to inspect the mip problem for a record
#' @examples
#' rules <- validator(x > 1)
#' data <- list(x = 0)
#'
#' mip <- inspect_mip(data, rules)
#' mip$rules
#' mip$mip_rules()
#' mip$objective
#' mip$to_lp()
#'
#' res <- mip$execute()
#' res
#' res$lp
#' @inheritParams locate_errors
inspect_mip <- function(data, rules, weight){
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
    data <- data[1,]
  }
  data <- as.list(data)

  mip <- miprules(rules)
  mip$set_values(data, weight)
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
