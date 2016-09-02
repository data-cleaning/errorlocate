#' Base class for class locate errors based on rules and data
#'
#' ErrorLocalizer can be used as a base class to implement a new error localization algorithm.
#' The derived class must implement two methods: \code{initialize}, which is called
#' before any error localization is done and \code{locate} which operates upon data. The
#' extra parameter \code{...} can used to supply algoritmic specific parameters.
#' @export
setRefClass("ErrorLocalizer",
  fields=list(
    "rules" = "validator",
    "used_rules" = "logical",
    "ref" = "data.frame"
  ),
  methods=list(
    initialize  = function(...){
      stop("Abstract class: not implemented. Please use an inherited class")
    },
    locate = function(data, ref=NULL, ...){
      stop("Implement locate on subclass of ErrorLocalizer")
    }
  )
)

#' Feligi-Holt Errorlocalizer
#'
#' Implementation of the Feligi-Holt algorithm using the \code{ErrorLocalizer} base class.
#' Given a set of validation rules and a dataset the Feligi-Holt algorithm finds for each record
#' the smallest (weighted) combination of variables that are erroneous (if any).
#'
#' \code{errorlocalizer} implements feligi holt using a MIP-solver. For problems in which
#' coefficients of the validation rules or the data are too different, you should consider scaling
#' the data.
#' @include mip.R
#' @exportClass FHLocalizer
#'
fh_localizer <-
  setRefClass("FHLocalizer",
    contains="ErrorLocalizer",
    fields = list(
      ._miprules = "ANY"
    ),
    methods = list(
      initialize = function(rules, ref = NULL){
        rules <<- rules
        ._miprules <<- miprules(rules)
      },
      locate = function(data, weight=NULL, ...){
        if (length(weight) == 0){
          weight <- matrix(1, nrow=nrow(data), ncol=ncol(data))
          colnames(weight) <- colnames(data)
        } else {
          stopifnot( names(weight) == names(data))
          if (is.null(dim(weight))){
            # use recycling to fill a weight matrix
            weight <- t(matrix(weight, nrow=ncol(data), ncol=nrow(data)))
            colnames(weight) <- colnames(data)
          }
          stopifnot(dim(weight) == dim(data))
        }

        rows <- seq_len(nrow(data))

        adapt <- matrix(NA, ncol=ncol(data), nrow=nrow(data))
        colnames(adapt) <- colnames(data)

        res <- sapply(rows, function(r){
          values <- data[r,,drop=FALSE]
          ._miprules$set_values(values, weight[r,])
          el <- ._miprules$execute()
          el$adapt
        })
        # TODO change to errorlocation
        errorlocation(values=t(res))
      }
    )
)

