#' Derive editmatrix with soft constraints based on boundaries of variables. This is a utility function that is used for
#' constructing a mip/lp problem.
#' @param E normalized \code{editmatrix}
#' @param prefix \code{character} used for naming dummy variables in matrix.
#' @export
soft_edits <- function(E, prefix="delta.", ...){
  UseMethod("soft_edits")
}
