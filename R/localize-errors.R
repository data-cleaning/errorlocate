
#' Locate errors
#'
#' Locate errors with validation rules.
#' @export
setGeneric("locate_errors", function(data, x, ref, weight, ...){
  standardGeneric("locate_errors")
})


#' @export
setMethod('locate_errors', c("data.frame", "validator"), function(data, x, ref, weight, ...){
  fh <- fh_localizer(x)
  locate_errors(data, fh, ref=ref, weight=weight, ...)
})


#' @export
setMethod('locate_errors', c("data.frame", "ErrorLocalizer"), function(data, x, ref, weight, ...){
  x$locate(data, weight, ...)
})


#' @export
setGeneric("errors_as_na", function(data, x, ...){
  standardGeneric("errors_as_na")
})

#' @export
setMethod('errors_as_na', c("data.frame", "validator"), function(data, x, ...){
  fh <- fh_localizer(x)
  errors_as_na(data, fh, ref, ...)
})

setMethod('errors_as_na', c("data.frame", "ErrorLocalizer"), function(data, x, ref=NULL, ...){
  el <- locate_errors(data, x, ref, ...)
  errors_as_na(data, el, ref, ...)
})

setMethod('errors_as_na', c("data.frame", "errorlocation"), function(data, x, ref, ...){
  stop("to be implemented")
})

.errors_as_na <- function(data, el){

}
