#' Error location object
#'
#' `errorlocation` contains the result of error localization.
#' It stores, per cell, whether a value is flagged as erroneous (`TRUE`), valid
#' (`FALSE`), or missing (`NA`).

#' \itemize{
#' \item A record-based error is restricted to one observation.
#' [errorlocate()] using the Fellegi-Holt algorithm assumes errors are
#' record-based.
#' \item A variable-based error is a flaw in a uni- or multivariate
#' distribution. Correcting it typically requires changing multiple observations
#' or aggregate totals.
#' }
#'
#'
#' Current implementation assumes record-based errors.
#' Retrieve error locations with [validate::values()], which returns a matrix
#' with the same dimensions as the checked `data.frame`.
#' For errors that are purely column-based, or dataset-based, `errorlocation`
#' returns a matrix with all
#' rows or cells set to `TRUE`.
#' [validate::values()] returns `NA` for missing values.

#' @section Fields:
#'
#' \itemize{
#'   \item `$errors`: `matrix` indicating which values are erroneous (`TRUE`),
#'   missing (`NA`) or valid (`FALSE`)
#'   \item `$weight`: The total weight per record. A weight of 0 means no errors were detected.
#'   \item `$status`: The [status][lpSolveAPI::solve.lpExtPtr] of the MIP solver for this record.
#'   \item `$duration`: The number of seconds for processing each record.
#' }
#'
#' @exportClass errorlocation
#' @family error finding
#' @rdname errorlocation
create_errorlocation <- setRefClass('errorlocation',
  fields=list(
    ._call = 'call',
    ._values = 'matrix',
    ._weight = 'numeric',
    ._status = 'integer',
    ._solution = 'logical',
    ._duration = 'numeric',
    ._suggestion = 'list',
    errors = function(){
      ._values
    },
    weight = function(){
      ._weight
    },
    status = function(){
      ._status
    },
    duration = function(){
      ._duration
    }
  ),
  methods=list(
    initialize = function( values=matrix()
                         , status=integer()
                         , weight= rep(1, NROW(values))
                         , suggestion=list()
                         , duration=numeric()
                         , solution = logical()
                         ){
      ._call <<- sys.call(-7)
      ._values <<- values
      ._status <<- status
      ._duration <<- duration
      ._weight <<- weight
      ._suggestion <<- suggestion
      ._solution <<- solution
    },
    show = function() {
      cat("call: ", deparse(._call), "\n")
      cat("located ", length(which(._values)), " error(s).\n")
      cat("located ", sum(is.na(._values)), " missing value(s).\n")
      n_fail <- sum(!._solution)
      if (n_fail > 0){
        cat("Failed to find a solution for ", n_fail, "record(s).\n")
      }
      cat("Use 'summary', 'values', '$errors' or '$weight', to explore and retrieve the errors.")
    }
  )
)

setMethod("values", "errorlocation", function(x, na_as_error = FALSE, ...){
  values <- x$._values
  if (isTRUE(na_as_error)){
    values[is.na(values)] <- TRUE
  }
  values
})

#' @method as.data.frame errorlocation
#' @export
as.data.frame.errorlocation <- function(x, row.names = NULL, optional = FALSE, ...){
  as.data.frame(x$._values, row.names = rownames, optional = optional, ...)
}

summary.errorlocation <- function(object, ...){
  errors <- values(object)
  #browser()
  variable <- data.frame (
    name = colnames(errors),
    errors = colSums(errors, na.rm=TRUE),
    missing = colSums(is.na(errors)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  variable <- variable[order(variable$errors, decreasing = TRUE),]
  structure(
    list(
      variable = variable,
      record_errors = table("errors" = rowSums(errors, na.rm = TRUE)),
      record_missing = table("missing" = rowSums(is.na(errors)))
    )
  , class="summary.errorlocation"
  )
}

#' @export
print.summary.errorlocation <- function(x, ...){
  cat("Variable:\n")
  print(x$variable)
  cat("Errors per record:\n")
  print(as.data.frame(x$record_errors, responseName="records"))
}

setMethod("summary", "errorlocation", summary.errorlocation)

#' Get location of removed errors from a 'cleaned' data set
#'
#' `errors_removed` retrieves the errors detected by [replace_errors()]
#' @param x `data.frame` that was checked for errors
#' @param ... not used
#' @return [errorlocation-class()] object
#' @example ./examples/replace_errors.R
#' @family error finding
#' @export
errors_removed <- function(x, ...){
  attr(x, "errorlocation")
}
# #' @export
# hist.errorlocation <- function(x, ...){
#   se <- summary(x)
#   hist(se$errors)
# }
