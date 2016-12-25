#' Error location object
#'
#' Error location contains the result of the error detection.
#' Errors can record based or variable based.

#' \itemize{
#' \item A record based error is restricted within one observation, which is the case. The default
#' \code{\link{errorlocate}} using the Felligi Holt algorithm assumes errors are record based.
#' \item A variable based error is a flaw in  uni- or multivariate
#'  distribution. To correct this error multiple observations or the aggregated number should be adjusted.
#' }
#'
#' Current implementation assumes that errors are record based. The error locations are a matrix of
#' rows and columns, with the same dimensions are the \code{data.frame} that was checked.
#' For errors that are purely column based, or dataset based, errorlocations will return a matrix with all
#' rows or cells set to \code{TRUE}.
#'
#' The information contained in error location object is: \code{values} for each variable is noted if the value is erroneous.
#'
#' @exportClass errorlocation
errorlocation <- setRefClass('errorlocation',
  fields=list(
    ._call = 'call',
    ._values = 'ANY',
    ._weight = 'numeric',
    ._status = 'list',
    ._suggestion = 'list',
    weight = function(){
      sum(._weight[._values])
    }
  ),
  methods=list(
    initialize = function(values=list(), status=list(), weight= rep(1, length(values)), suggestion=list()){
      ._call <<- sys.call(-5)
      ._values <<- values
      ._status <<- status
      ._weight <<- weight
      ._suggestion <<- suggestion
    },
    show = function() {
      cat("call: ", deparse(._call), "\n")
      cat("errors:\n")
      print(._values)
      cat("weight:\n")
      print(._weight)
    }
  )
)

setMethod("values", "errorlocation", function(x, ...){
  x$._values
})

#' @export
as.data.frame.errorlocation <- function(x, ...){
  as.data.frame(x$._values)
}

#' Summary of errorlocation object
#'
#' Summary of errorlocation
#' @export
#' @inheritParams base::summary
summary.errorlocation <- function(object, ...){
  errors <- values(object)
  list(
    variable_errors = colSums(errors),
    record_errors = table("#errors" = rowSums(errors))
  )
}

#' @export
hist.errorlocation <- function(x, ...){
}
