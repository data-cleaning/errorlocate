#' Base class for class locate errors based on rules and data
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

##
#' @include mip.R
#' @exportClass FHLocalizer
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
      locate = function(data, weight, ...){

        if (missing(weight)){
          weight <- matrix(1, nrow=nrow(data), ncol=ncol(data))
          colnames(weight) <- colnames(data)
        } else {
          stopifnot( names(weight) == names(data))
          if (is.null(dim(weight))){
            # use recycling to fill a weight matrix
            weight <- t(matrix(weight, nrow=ncol(data), ncol=nrow(data)))
            colnames(weight) <- colnames(data)
          }
        }

        rows <- seq_len(nrow(data))
        res <- sapply(rows, function(r){
          values <- data[r,]
          ._miprules$set_values(values)
          el <- ._miprules$execute()
          el$adapt
        })
        # TODO change to errorlocation
        # browser()
        errorlocation(values=t(res))
      }
    )
)

