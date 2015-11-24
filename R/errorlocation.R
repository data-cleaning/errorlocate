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
