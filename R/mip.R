#' Create a mip object from a validator object
#'
#' Create a mip object from \code{\link{validator}} object.
#' This is a utility class that translates a validor object into a mixed integer problem that
#' can be solved.
#' Most users should use \code{\link{locate_errors}} which will handle all translation and execution
#' automatically. This class is provided so users can implement or derive a alternative solution.
#'
#' @section Methods:
#' The \code{MipRules} class contains the following methods:
#' \itemize{
#'   \item \code{$execute} calls the mip solver to execute the rules.
#'   \item \code{$to_lp}: transforms the object into a lp_solve object
#'   \item \code{$is_infeasible} Checks if the current system of mixed integer rules is feasible.
#'   \item \code{$set_values}: set values and weights for variables (determines the objective function).
#' }
#'
#' @exportClass MipRules
#' @examples
#' rules <- validator(x > 1)
#' mr <- miprules(rules)
#' mr$to_lp()
#' mr$set_values(list(x=0, weight=list(x=1)))
#' mr$execute()
#' @export miprules
miprules <- setRefClass("MipRules",
   fields = list(
     rules         = "validator",
     objective     = "numeric",
     ._lin_rules   = "list",
     ._cat_rules   = "list",
     ._cond_rules  = "ANY",
     ._value_rules = "list",
     ._lp          = "ANY"
   ),
   methods = list(
     initialize = function(rules){
       rules <<- rules
       objective <<- objective
       ._lin_rules <<- lin_as_mip_rules(rules)
       ._cat_rules <<- cat_as_mip_rules(rules)
       ._cond_rules <<- cond_as_mip_rules(rules)
       #TODO implement cond_as_mip_rules
     },
     mip_rules = function(){
       c(._lin_rules, ._cat_rules, ._cond_rules, ._value_rules)
     },
     set_values = function(values, weights){
       if (missing(values) || length(values) == 0){
         objective <<- numeric()
         ._value_rules <<- list()
         invisible()
       }
       if (missing(weights)){
         weights <- rep(1, length(values))
         names(weights) <- names(values)
       }
       ._value_rules <<- expect_values(values, weights)
#       weights <- weights + runif(length(weights), max = 1e-3)
       weights <- add_noise(weights)
       objective <<- setNames(weights, paste0(".delta_", names(weights)))
     },
     to_lp = function(){
       translate_mip_lp(mip_rules(), objective)
     },
     execute = function(){
       # TODO see if this can be executed in parallel.
       #browser()
       lp <- translate_mip_lp(mip_rules(), objective)
       #TODO set timer, duration etc.
       s <- solve(lp)
       values <- lpSolveAPI::get.variables(lp)
       names(values) <- colnames(lp)
       adapt <- values[names(objective)] == 1
       names(adapt) <- gsub(".delta_", "", names(adapt))
       #TODO improve the return values based on value of s
       list(
         s = s,
         values = values,
         lp = lp,
         adapt = adapt,
         errors = adapt
       )
     },
     is_infeasible = function(){  # since we only check a subset of the rules,
       mr <- .self$mip_rules()    # we can only detect infeasiblity, not feasiblity
       vars <- get_mr_vars(mr)

       obj <- rep(1, length(vars))
       names(obj) <- vars

       lp <- translate_mip_lp(mr, obj)
       i <- lpSolveAPI::solve.lpExtPtr(lp)
       feasible <- switch( as.character(i),
          "0" = TRUE,  # optimal solution found (so feasible)
          "1" = TRUE,  # sub optimal solution (so feasible)
          "2" = FALSE, # infeasible
          "3" = TRUE,  # unbounded (so feasible)
          "4" = TRUE,  # degenerate (so feasible)
          "5" = NA,    # numerical failure, so unknown
          "6" = NA,    # process aborted
          "7" = NA,    # timeout
          "9" = TRUE,  # presolved
         "10" = FALSE, # branch and bound failed
         "11" = FALSE, # branch and bound stopped
         "12" = TRUE,  # a feasible branch and bound found
         "13" = FALSE, # no feasible branch and bound found
         FALSE
       )
       !feasible
     },
     show = function(){
       mr <- mip_rules()
       cat("Mip rules:\n")
       cat(paste(sapply(mr, as.character), collapse = "\n"))
     }
   )
)


### testing
# v <- validator(x>3, y>2)
# m <- mip(v)
