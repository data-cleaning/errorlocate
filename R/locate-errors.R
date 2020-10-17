#' Locate errors in data
#'
#' Locate erronuous fields in rows of data using validation rules or a specific
#' errorlocalizer object. This method returns found errors, according to the specified
#' method \code{x}. If these errors are to be removed automatically
#' use method \code{\link{replace_errors}}.
#' @param data data to be checked
#' @param x validation rules or errorlocalizer object to be used for finding
#' possible errors.
#' @param ref \code{data.frame} optional reference data to be used in the rules checking
#' @param weight \code{numeric} optional weight vector to be used in the error localization.
#' @param ... optional parameter to be used by a specific method
#' @param timeout maximum number of seconds that the localizer should use per record.
#' @return \code{\link{errorlocation-class}} object describing the errors found.
#'
#' @example examples/locate_errors.R
#' @family error finding
#' @export
setGeneric("locate_errors", function(data, x, ..., timeout = 60){
  standardGeneric("locate_errors")
})

#' @export
#' @rdname locate_errors
setMethod('locate_errors', signature = c("data.frame", "validator"), function(data, x, weight=NULL, ref=NULL, ..., timeout=60){
  fh <- fh_localizer(x)
  locate_errors(data=data, fh, ref=ref, weight=weight, ..., timeout = timeout)
})


#' @export
#' @rdname locate_errors
setMethod('locate_errors', signature = c("data.frame", "ErrorLocalizer"), function(data, x, weight=NULL, ref=NULL, ..., timeout=60){
  x$locate(data=data, weight=weight, ..., timeout=timeout)
})
