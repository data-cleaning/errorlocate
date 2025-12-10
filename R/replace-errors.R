#' Replace erroneous fields with NA or a suggested value
#'
#' Find erroneous fields using [locate_errors()] and replace these
#' fields automatically with NA or a suggestion that is provided by the error detection algorithm.
#'
#' Note that you can also use the result of [locate_errors()] with `replace_errors`.
#' When the procedure takes a long time and `locate_errors` was called previously
#' this is the preferred way, because otherwise `locate_errors` will be executed again.
#' The errors that were removed from the `data.frame` can be retrieved with the function
#' [errors_removed()]. For more control over error localization see [locate_errors()].
#'
#' `replace_errors` has the same parallelization options as [locate_errors()] (see there).
#'
#' @note In general it is better to replace the erroneous fields with `NA` and apply a proper
#' imputation method. Suggested values from the error localization method may introduce an undesired bias.
#'
#' @param data data to be checked
#' @param x [validate::validator()] or `errorlocation` object.
#' If an `errorlocation` is already available (through [locate_errors()]) this
#' is more efficient.
#' @param ref optional reference data set
#' @param ... these parameters are handed over to [locate_errors()]
#' @param cl optional cluster for parallel execution (see details)
#' @param Ncpus number of nodes to use. (see details)
#' @param value `NA`
#' @seealso [errorlocation-class()]
#' @export
#' @return `data` with erroneous values removed.
#' @example ./examples/replace_errors.R
#' @family error finding
setGeneric("replace_errors", function( data
                                     , x
                                     , ref=NULL, ...
                                     , cl = NULL
                                     , Ncpus = getOption("Ncpus", 1)
                                     , value = c("NA", "suggestion")){
  standardGeneric("replace_errors")
})

#' @export
#' @rdname replace_errors
setMethod('replace_errors', c("data.frame", "validator")
         , function(data, x, ref=NULL, ..., cl = NULL
                    , Ncpus = getOption("Ncpus", 1)
                    , value = c("NA", "suggestion")){
  fh <- fh_localizer(x)
  replace_errors(data, fh, ref, ..., cl = cl, Ncpus = Ncpus)
})

#' @export
#' @rdname replace_errors
setMethod('replace_errors', c("data.frame", "ErrorLocalizer")
         , function(data, x
                   , ref = NULL
                   , ...
                   , cl = NULL
                   , Ncpus = getOption("Ncpus", 1)
                   , value = c("NA", "suggestion")){
  el <- locate_errors(data, x, ref, ..., cl = cl, Ncpus = Ncpus)
  replace_errors(data, el, ref, ..., cl = cl, Ncpus = Ncpus)
})

#' @export
#' @rdname replace_errors
setMethod('replace_errors', c("data.frame", "errorlocation")
         , function( data, x
                   , ref = NULL,
                   ...
                   , cl = NULL # not used...
                   , Ncpus = 1 # not used
                   , value = c("NA", "suggestion")){
    value <- switch ( match.arg(value),
        "NA" = is.na(data) <- values(x, na_as_error = TRUE)
    )
    attr(data,"errorlocation") <- x
    data
})
