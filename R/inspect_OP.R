#' inspect the mip problem formulation
#'
#' Utility function to inspect the mip problem for a record as an [ROI::OP()].
#' `inspect_OP` can
#' be used as a "drop-in" replacement for [locate_errors()], but works on the
#' first record.
#'
#' It may sometimes be handy to find out what is happening exactly with a record.
#' See the example section for finding out what to do with inspect_OP.
#' @example ./examples/inspect_op.R
#' @family Mixed Integer Problem
#' @export
#' @inheritParams locate_errors
inspect_OP <- function(data, x, weight, ...){
  mip <- inspect_mip(data = data, x = x, weight = weight, ...)
  mip$to_OP()
}
