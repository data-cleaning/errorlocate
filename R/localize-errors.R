
#' @export
setGeneric("locate_errors", function(data, x, weight, ...){
  standardGeneric("locate_errors")
})


#' @export
setMethod('locate_errors', c("data.frame", "validator"), function(data, x, weight, ...){
  stop("Not yet implemented")
})


#' @export
setMethod('locate_errors', c("data.frame", "ErrorLocalizer"), function(data, x, weight, ...){
  x$locate(data, weight, ...)
})


#' utility function that replaces errors with NA
errors_as_na <-  function(data, x, ref=NULL, ...){
}

