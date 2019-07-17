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
    locate = function(data, ref=NULL, ..., timeout=60){
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
#' @note Most users do not need this class and can use \code{\link{locate_errors}}.
#'
#' \code{errorlocalizer} implements feligi holt using a MIP-solver. For problems in which
#' coefficients of the validation rules or the data are too different, you should consider scaling
#' the data.
#' @include MipRules.R
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
      locate = function(data, weight=NULL, add_noise = TRUE, ..., timeout=60){
        if (length(weight) == 0){
          weight <- matrix(1, nrow=nrow(data), ncol=ncol(data))
          colnames(weight) <- colnames(data)
        } else {
          if (is.null(dim(weight)) && length(weight) == ncol(data)){
            # use recycling to fill a weight matrix
            weight <- t(matrix(weight, nrow=ncol(data), ncol=nrow(data)))
            colnames(weight) <- colnames(data)
          }
          stopifnot(dim(weight) == dim(data))
          if (is.null(colnames(weight))){
            colnames(weight) <- colnames(data)
          }
          stopifnot(names(weight) == names(data))
        }
        #browser()

        # if (isTRUE(add_noise)){
        #   weight <- add_noise(weight)
        # }

        rows <- seq_len(nrow(data))

        # TODO add suggestions, status and progress bar
        if (interactive()) {
          pb <- utils::txtProgressBar(min = 0, max=nrow(data))
        }
        res <- sapply(rows, function(r){
          # cat(".")
          values <- data[r,,drop=FALSE]
          ._miprules$set_values(values, weight[r,])
          el <- ._miprules$execute(timeout=timeout, ...)
          adapt <- el$adapt
          rm(el)
          gc()
          if (interactive()){
            value <- 1 + pb$getVal()
            utils::setTxtProgressBar(pb, value)
          }
          adapt
        })
        if(interactive()){ close(pb) }
        dim(res) <- dim(weight)[2:1]
        adapt <- t(res)
        colnames(adapt) <- colnames(weight)

        weight_per_record <- as.numeric(tcrossprod(adapt, weight))

        is.na(adapt) <- is.na(data)
        #browser()
        create_errorlocation(
          values = adapt,
          weight = weight_per_record
        )
      }
    )
)

