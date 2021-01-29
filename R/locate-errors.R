#' Locate errors in data
#'
#' Locate erroneuous fields in rows of data using validation rules or a specific
#' errorlocalizer object. This method returns found errors, according to the specified
#' method `x`.
#' Use method [replace_errors()], to automatically remove these errors.
#'
#' `locate_errors` uses lpSolveAPI to formulate and solve a mixed integer problem.
#' This solver has many options:  [lpSolveAPI::lp.control.options]. Noteworthy
#' options to be used are:
#'
#' - `timeout`: restricts the time the solver spends on a record (seconds)
#' - `break.at.value`: set this to minimum weight + 1 to improve speed.
#'
#' `locate_errors` can be run on multiple cores using R package `parallel`.
#'
#'  - The easiest way to use the parallel option is to set `Ncpus` to the number of
#' desired cores, @seealso [parallel::detectCores()].
#'
#' - Alternatively one can create a cluster object ([parallel::makeCluster()])
#' and use `cl` to pass the cluster object.
#'
#' - Or set `cl` to an integer which results in [parallel::mclapply()], which only works
#' on non-windows.
#'
#' @param data data to be checked
#' @param x validation rules or errorlocalizer object to be used for finding
#' possible errors.
#' @param ref `data.frame` optional reference data to be used in the rules checking
#' @param weight `numeric` optional weight vector to be used in the error localization.
#' @param ... optional parameters that are passed to [lpSolveAPI::lp.control()] (see details)
#' @param cl optional parallel / cluster.
#' @param Ncpus number of nodes to use. See details
#' @param timeout maximum number of seconds that the localizer should use per record.
#' @return [errorlocation-class()] object describing the errors found.
#'
#' @example examples/locate_errors.R
#' @family error finding
#' @export
setGeneric("locate_errors", function( data, x, ..., cl = NULL
                                    , Ncpus = getOption("Ncpus", 1), timeout = 60
                                    ){
  standardGeneric("locate_errors")
})

#' @export
#' @rdname locate_errors
setMethod('locate_errors', signature = c("data.frame", "validator")
        , function(data, x, weight=NULL, ref=NULL, ..., cl = NULL
                   , Ncpus = getOption("Ncpus", 1), timeout=60){
  fh <- fh_localizer(x)
  locate_errors(data=data, fh, ref=ref, weight=weight, ..., cl = cl, Ncpus = Ncpus, timeout = timeout)
})


#' @export
#' @rdname locate_errors
setMethod('locate_errors', signature = c("data.frame", "ErrorLocalizer")
        , function(data, x, weight=NULL, ref=NULL, ..., cl = NULL
                  , Ncpus = getOption("Ncpus", 1), timeout=60){
  x$locate(data=data, weight=weight, ..., cl = cl, Ncpus = Ncpus, timeout=timeout)
})
