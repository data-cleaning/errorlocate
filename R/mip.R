#' Create mip object
#'
#' Create mip object
#' @export mip
#' @export
mip <- setRefClass("Mip",
   fields = list(
     expressions = "validator",
     rules = "list",
     linear_coefficients = "list",
     cat_coefficients    = "list",
     objective = "numeric"
   ),
   methods = list(
     initialize = function(expressions, objective=numeric()){
       expressions <<- expressions
       objective <<- objective
     },
     used_expressions = function(){
       expressions$is_linear() | is_categorical(expressions)
     }
   )
)


### testing
v <- validator(x>3, y>2)
m <- mip(v)
