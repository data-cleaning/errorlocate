#' Base class for class locate errors based on rules and data
#' @export
setRefClass("ErrorLocalizer",
  fields=list(),
  methods=list(
    initialize  = function(...){
      stop("Abstract class: not implemented. Please use an inherited class")
    },
    locate = function(data, rules, ...){
      stop("Implement locate on subclass")
    }
  )
)


#' @export
setGeneric("locate_errors", function(x, data, rules, ...){
  standardGeneric("locate_errors")
})


#' @export
setMethod('locate_errors', c("ErrorLocalizer"), function(x, data, rules, ...){
  x$locate(data, rules, ...)
})


##
