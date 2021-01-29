#' Error location object
#'
#' Errorlocation contains the result of a error detection.
#' Errors can record based or variable based.

#' \itemize{
#' \item A record based error is restricted within one observation.
#' [errorlocate()] using the Felligi Holt algorithm assumes errors are record based.
#' \item A variable based error is a flaw in  uni- or multivariate
#'  distribution. To correct this error multiple observations or the aggregated number should be adjusted.
#' }
#'
#'
#' Current implementation assumes that errors are record based. The error locations can be retrieved
#' using the method [values()] and are a matrix of
#' rows and columns, with the same dimensions are the `data.frame` that was checked.
#' For errors that are purely column based, or dataset based, errorlocations will return a matrix with all
#' rows or cells set to `TRUE`.
#' The [values()] return `NA` for missing values.

#' @section Fields:
#'
#' \itemize{
#'   \item `$errors`: `matrix` indicating which values are erronuous (`TRUE`),
#'   missing (`NA`) or valid (`FALSE`)
#'   \item `$weight`: The total weight per record. A weight of 0 means no errors were detected.
#'   \item `$status`: The [status][lpSolveAPI::solve.lpExtPtr] of the mip solver for this record.
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
