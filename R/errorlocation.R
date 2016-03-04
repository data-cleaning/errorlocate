#' Error location object
#'
#' Error location contains the result of the error detection
#' Errors can variable based, and/or record based. A variable based error is a flaw in  uni- or multivariate
#' distribution. To correct this error multiple observations or the aggregated number would have to be adjusted.
#' A record based #' errors on the other hand is restricted within one observation.
#'
#' Current implementation assumes that they are record based: errors are a matrix of rows and columns.
#' However it is thinkable that errors are purely column based, or dataset based. This
#' is currently implemented by settings all rows or all values to TRUE.
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
      cat("errors: ", as.character(._values), "\n")
      cat("weight: ", weight, "\n")
    }
  )
)

setMethod("values", "errorlocation", function(x, ...){
  x$._values
})
