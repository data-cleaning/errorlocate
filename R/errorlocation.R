#' Error location object
#'
#' Error location contains the result of the error detection.
#' Errors can record based or variable based.

#' \itemize{
#' \item A record based error is restricted within one observation.
#' \code{\link{errorlocate}} using the Felligi Holt algorithm assumes errors are record based.
#' \item A variable based error is a flaw in  uni- or multivariate
#'  distribution. To correct this error multiple observations or the aggregated number should be adjusted.
#' }
#'
#' Current implementation assumes that errors are record based. The error locations can be retrieved
#' using the method \code{values} and are a matrix of
#' rows and columns, with the same dimensions are the \code{data.frame} that was checked.
#' For errors that are purely column based, or dataset based, errorlocations will return a matrix with all
#' rows or cells set to \code{TRUE}.
#' The \code{values} return \code{NA} for values that are
#'
#' The information contained in error location object is: \code{values} for each variable is noted if the value is erroneous.
#'
#' @export errorlocation
errorlocation <- setRefClass('errorlocation',
  fields=list(
    ._call = 'call',
    ._values = 'matrix',
    ._weight = 'numeric',
    ._status = 'list',
    ._suggestion = 'list',

    weight = function(){
      ._weight[._values]
    }
  ),
  methods=list(
    initialize = function(values=matrix(), status=list(), weight= rep(1, length(values)), suggestion=list()){
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
      cat("Use 'summary', 'values' or '$weight', to explore and retrieve the errors.")
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

#' @export
as.data.frame.errorlocation <- function(x, ...){
  as.data.frame(x$._values)
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

# #' @export
# hist.errorlocation <- function(x, ...){
#   se <- summary(x)
#   hist(se$errors)
# }
