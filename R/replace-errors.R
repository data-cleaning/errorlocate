#'  Replace erroneous fields with NA or a suggested value
#'
#' Find erronous fields using \code{\link{locate_errors}} and replace these
#' fields automatically with NA or a suggestion that is provided by the error detection algorithm.
#' @note In general it is better to replace the erronuous fields with \code{NA} and apply a proper
#' imputation methods. Suggested values from the error localization method may introduce an unwanted bias.
#'
#' The errors that were removed from the \code{data.frame} can be retrieved with the function
#'  \code{\link{errors_removed}}. For more control over error localization see \code{\link{locate_errors}}.
#' @param data data to be checked
#' @param x \code{\link{validator}} object
#' @param ref optional reference data set
#' @param ... these parameters are handed over to \code{\link{locate_errors}}
#' @param value \code{NA}
#' @seealso \code{\link{errorlocation-class}}
#' @export
#' @return \code{data} with erronuous values removed.
#' @example ./examples/replace_errors.R
setGeneric("replace_errors", function( data
                                     , x
                                     , ref=NULL, ...
                                     , value = c("NA", "suggestion")){
  standardGeneric("replace_errors")
})

#' @export
#' @rdname replace_errors
setMethod('replace_errors', c("data.frame", "validator")
         , function(data, x, ref=NULL, ..., value = c("NA", "suggestion")){
  fh <- fh_localizer(x)
  replace_errors(data, fh, ref, ...)
})

#' @export
#' @rdname replace_errors
setMethod('replace_errors', c("data.frame", "ErrorLocalizer")
         , function(data, x
                   , ref = NULL
                   , ...
                   , value = c("NA", "suggestion")){
  el <- locate_errors(data, x, ref, ...)
  replace_errors(data, el, ref, ...)
})

#' @export
#' @rdname replace_errors
setMethod('replace_errors', c("data.frame", "errorlocation")
         , function( data, x
                   , ref = NULL,
                   ...
                   , value = c("NA", "suggestion")){
    value <- switch ( match.arg(value),
        "NA" = is.na(data) <- values(x)
    )
    attr(data,"errorlocation") <- x
    data
})
