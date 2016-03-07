
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
#' @example examples/locate_errors.R
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

#'  Replace erroneous fields with NA or suggested value
#'
#'  @param data data to be checked
#'  @param x \code{\link{validator}} object
#'  @param ref optional reference data set
#'  @param ... other datasets to be used...
#'  @param value \code{NA}
#'
#' Find erronous fields using \code{\link{locate_errors}} and replace these
#' fields automatically with NA.
#'
#' @seealso locate_errors
#' @export
setGeneric("replace_errors", function( data, x
                                     , ref=NULL, ...
                                     , value = c("NA", "suggestion")){
  standardGeneric("replace_errors")
})

#' @export
setMethod('replace_errors', c("data.frame", "validator")
         , function(data, x, ref=NULL, ..., value = c("NA", "suggestion")){
  fh <- fh_localizer(x)
  replace_errors(data, fh, ref, ...)
})

setMethod('replace_errors', c("data.frame", "ErrorLocalizer")
         , function(data, x
                   , ref = NULL
                   , ...
                   , value = c("NA", "suggestion")){
  el <- locate_errors(data, x, ref, ...)
  replace_errors(data, el, ref, ...)
})

setMethod('replace_errors', c("data.frame", "errorlocation")
         , function( data, x
                   , ref = NULL,
                   ...
                   , value = c("NA", "suggestion")){
    x$adapt
})

