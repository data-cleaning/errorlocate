#'  Replace erroneous fields with NA or a suggested value
#'
#' Find erronous fields using \code{\link{locate_errors}} and replace these
#' fields automatically with NA. The error location algorithm may provide a suggestion which
#' validates the record against the rules.
#' @note In general it is better to replace the erronuous fields with \code{NA} and apply a proper
#' imputation methods. Suggested values from the error localization method may introduce an unwanted bias.
#' @param data data to be checked
#' @param x \code{\link{validator}} object
#' @param ref optional reference data set
#' @param ... other datasets to be used...
#' @param value \code{NA}
#' @seealso locate_errors
#' @export
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

    data
})
