#' Find errors in data
#'
#' Locate fields in a data.frame that are likely erroneous under a set of
#' validation rules. The method returns an [errorlocation-class()] object,
#' computed with localizer `x`.
#'
#' Use [replace_errors()] to remove flagged fields, typically by setting them to
#' `NA`. Use [base::set.seed()] beforehand to make calls reproducible.
#'
#' Use an `Inf` `weight` specification to fix variables that should not be
#' changed.
#' See [expand_weights()] for more details.
#'
#' `locate_errors` uses `lpSolveAPI` to formulate and solve a mixed integer
#' problem. See the vignettes for details.
#' The solver has many options, see [lpSolveAPI::lp.control.options()].
#' Noteworthy options include:
#'
#' - `timeout`: restricts the time the solver spends on a record (seconds)
#' - `break.at.value`: set this to `minimum weight + 1` to improve speed.
#' - `presolve`: default in `errorlocate` is `"rows"`. Set to `"none"` when you have
#' solutions where all variables are deemed wrong.
#'
#' `locate_errors` can run on multiple cores using package `parallel`.
#'
#' - The easiest option is setting `Ncpus` to the number of desired cores.
#'
#' - Alternatively one can create a cluster object ([parallel::makeCluster()])
#' and use `cl` to pass the cluster object.
#'
#' - Or set `cl` to an integer which results in [parallel::mclapply()], which only works
#' on non-Windows systems.
#'
#' @param data data to be checked
#' @param x validation rules or errorlocalizer object to be used for finding
#' possible errors.
#' @param ref `data.frame` optional reference data to be used in the rules checking
#' @param weight `numeric` optional weight specification to be used in the
#' error localization (see [expand_weights()]).
#' @param ... optional parameters that are passed to [lpSolveAPI::lp.control()] (see details)
#' @param cl optional parallel / cluster.
#' @param Ncpus number of nodes to use. See details
#' @param timeout maximum number of seconds that the localizer should use per record.
#' @return [errorlocation-class()] object describing the errors found.
#'
#' @seealso [parallel::detectCores()]
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
