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
      locate = function(data, ...){
        row_count <- seq_len(nrow(data))
        res <- sapply(row_count, function(r){
          values <- data[r,]
          ._miprules$set_values(values)
          el <- ._miprules$execute()
        })
        #TODO change to errorlocation
        res
      }
    )
)

