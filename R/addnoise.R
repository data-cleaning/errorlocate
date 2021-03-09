#' Add (a small amount of) noise
#'
#' Utility function to add some small positive noise to weights.
#' This is mainly done to randomly choose between solutions
#' of equal weight. Without adding noise to weights lp solvers may return
#' an identical solution over and over while there are multiple solutions of equal weight.
#' The generated noise is positive to prevent that weights will be zero or negative.
#'
#' When no `max_delta` is supplied, add_noise will use the minimum difference
#' larger than zero divided by the `length(x)`.
#' @param x `numeric` vector or matrix. When `x` is a matrix, the function
#' will be applied to each row of the matrix.
#' @param max_delta when supplied noise will be drawn from `[0,max_delta]`
#' otherwise see details
#' @param ... currently not used
#' @return `numeric` vector/matrix with noise applied.
#' @export
add_noise <- function(x, max_delta = NULL, ...){
  if (is.matrix(x)){
    return(t(apply(x, 1, add_noise, max_delta=max_delta, ...)))
  }

  N <- length(x)
  if (is.null(max_delta)){
    x_f <- x[is.finite(x)]
    ds <- diff(sort(x_f))
    ds <- c(ds[ds > .Machine$double.eps])
    max_delta <- min(ds, min(x_f)) / N
  }
  x + runif(N, max = max_delta)
}


# testing 1,2 3

#add_noise(c(1,1.4))
