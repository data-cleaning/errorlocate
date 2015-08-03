#' Create mip object
#'
#' Create mip object
#' @export mip
#' @export
mip <- setRefClass("MipRules",
   fields = list(
     rules         = "validator",
     objective     = "numeric",
     ._lin_rules   = "list",
     ._cat_rules   = "list",
     ._cond_rules  = "list",
     ._value_rules = "list",
     ._lp          = "ANY"
   ),
   methods = list(
     initialize = function(rules){
       rules <<- rules
       objective <<- objective
       ._lin_rules <<- lin_as_mip_rules(rules)
       ._cat_rules <<- cat_as_mip_rules(rules)
       #TODO implement cond_as_mip_rules
     },
     mip_rules = function(){
       c(._lin_rules, ._cat_rules, ._cond_rules, ._value_rules)
     },
     set_values = function(values, weights){
       if (missing(weights)){
         weights <- rep(1, length(values))
       }
       ._value_rules <<- expect_values(values, weights)
       objective <<- setNames(weights, paste0(".delta_", names(weights)))
     },
     is_feasible = function(){
       #browser()
       mr <- .self$mip_rules()
       vars <- get_mr_vars(mr)

       obj <- rep(1, length(vars))
       names(obj) <- vars

       lp <- translate_mip_lp(mr, obj)
       i <- lpSolveAPI::solve.lpExtPtr(lp)
       feasible <- switch( as.character(i),
          "0" = TRUE,
          "1" = TRUE,
          "3" = TRUE,
          FALSE
       )
       feasible
     },
     solution_values = function(){
     }
   )
)


### testing
# v <- validator(x>3, y>2)
# m <- mip(v)
