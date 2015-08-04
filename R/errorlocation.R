#' Error location
#'
#' Errors can variable based, and/or record based
#' Current implementation assumes that it  is record based: errors are a matrix of rows and columns.
#' However it is thinkable that errors are purely column based, or dataset based. This
#' is currently implemented by settings all rows or all values to TRUE.
#' @exportClass errorlocation
errorlocation <- setRefClass('errorlocation',
  fields=list(
    ._call = 'call',
    ._values = 'ANY',
    ._status = 'list'
  ),
  methods=list(
    initialize = function(values=list(), status=list()){
      ._call <<- sys.call(-5)
      ._values <<- values
      ._status <<- status
    },
    show = function() {
      cat("call: ", deparse(._call), "\n")
      cat("errors: ", as.character(._values), "\n")
    }
  )
)

setMethod("values", "errorlocation", function(x, ...){
  x$._values
})
