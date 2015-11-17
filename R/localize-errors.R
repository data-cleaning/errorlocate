
#' Locate errors in data
#'
#' @param data data to be checked
#' @param x validation rules or errorlocalizer object to be used for finding
#' possible errors.
#' @return \code{\link{error}} object describing the errors found.
#'
#' Locate erronuous fields in rows of data using validation rules or a specific
#' errorlocalizer object. This method returns the errors, according to the specified
#' method \code{x}, that were found. If these errors are to be removed automatically
#' the method \code{\link{errors_as_na}} should be used.
#' @seealso locate_errors
#' @export
setGeneric("locate_errors", function(data, x, ...){
  standardGeneric("locate_errors")
})

#' @export
setMethod('locate_errors', signature = c("data.frame", "validator"), function(data, x, ref=NULL, weight=NULL, ...){
  fh <- fh_localizer(x)
  locate_errors(data=data, fh, ref=ref, weight=weight, ...)
})


#' @export
setMethod('locate_errors', signature = c("data.frame", "ErrorLocalizer"), function(data, x, weight=NULL, ref=NULL, ...){
  x$locate(data=data, weight=weight, ...)
})

#' Set erroneous fields to NA
#'
#'  @param data data to be checked
#'  @param x \code{\link{validator}} object
#'
#' Find erronous fields using \code{\link{locate_errors}} and replace these
#' fields automatically with NA.
#'
#' @seealso locate_errors
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
