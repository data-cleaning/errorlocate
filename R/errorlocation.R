#' Error location object
#'
#' Errorlocation contains the result of a error detection.
#' Errors can record based or variable based.

#' \itemize{
#' \item A record based error is restricted within one observation.
#' \code{\link{errorlocate}} using the Felligi Holt algorithm assumes errors are record based.
#' \item A variable based error is a flaw in  uni- or multivariate
#'  distribution. To correct this error multiple observations or the aggregated number should be adjusted.
#' }
#'
#'
#' Current implementation assumes that errors are record based. The error locations can be retrieved
#' using the method \code{\link{values}} and are a matrix of
#' rows and columns, with the same dimensions are the \code{data.frame} that was checked.
#' For errors that are purely column based, or dataset based, errorlocations will return a matrix with all
#' rows or cells set to \code{TRUE}.
#' The \code{\link{values}} return \code{NA} for missing values.

#' @section Fields:
#'
#' \itemize{
#'   \item \code{$errors}: \code{matrix} indicating which values are erronuous (\code{TRUE}),
#'   missing (\code{NA}) or valid (\code{FALSE})
#'   \item \code{$weight}: The total weight per record. A weight of 0 means no errors were detected.
#' }
#'
#' @exportClass errorlocation
#' @rdname errorlocation
create_errorlocation <- setRefClass('errorlocation',
  fields=list(
    ._call = 'call',
    ._values = 'matrix',
    ._weight = 'numeric',
    ._status = 'list',
    ._suggestion = 'list',
    errors = function(){
      ._values
    },
    weight = function(){
      ._weight
    }
  ),
  methods=list(
    initialize = function(values=matrix(), status=list(), weight= rep(1, NROW(values)), suggestion=list()){
      ._call <<- sys.call(-5)
      ._values <<- values
      ._status <<- status
      ._weight <<- weight
      ._suggestion <<- suggestion
    },
    show = function() {
      cat("call: ", deparse(._call), "\n")
      cat("located ", length(which(._values)), " error(s).\n")
      cat("located ", sum(is.na(._values)), " missing value(s).\n")
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
#' \code{errors_removed} retrieves the errors detected by \code{\link{replace_errors}}
#' @param x \code{data.frame} that was checked for errors
#' @param ... not used
#' @return \code{\link{errorlocation-class}} object
#' @export
errors_removed <- function(x, ...){
  attr(x, "errorlocation")
}
# #' @export
# hist.errorlocation <- function(x, ...){
#   se <- summary(x)
#   hist(se$errors)
# }
