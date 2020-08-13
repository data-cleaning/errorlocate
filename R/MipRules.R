#' Create a mip object from a validator object
#'
#' Create a mip object from \code{\link{validator}} object.
#' This is a utility class that translates a validor object into a mixed integer problem that
#' can be solved.
#' Most users should use \code{\link{locate_errors}} which will handle all translation and execution
#' automatically. This class is provided so users can implement or derive an alternative solution.
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
#' mr$set_values(c(x=0), weights=c(x=1))
#' mr$execute()
#' @export miprules
miprules <- setRefClass("MipRules",
   fields = list(
     rules         = "validator",
     objective     = "numeric",
     ._miprules   = "list",
     ._value_rules = "list",
     ._vars        = "character",
     ._vars_num    = "character",
     ._ignored     = "ANY",
     ._lp          = "ANY"
   ),
   methods = list(
     initialize = function(rules){
       rules <<- rules
       objective <<- objective
       ._miprules <<- to_miprules(rules)

       var_num <- sapply(._miprules, function(mr){
                  names(mr$type)[mr$type == "double"]})
       ._vars_num <<- unique(as.character(var_num))
       ._vars <<- validate::variables(rules)
       # remove variables that are not in data.frame but in the environment
       ._vars <<- ._vars[!sapply(._vars, exists)]
     },
     mip_rules = function(){
       c(._miprules, ._value_rules)
     },
     set_values = function(values, weights){
       #browser()
       if (missing(values) || length(values) == 0){
         objective <<- numeric()
         ._value_rules <<- list()
         return(invisible())
       }

       missing_vars <- ._vars[!._vars %in% names(values)]
       if (length(missing_vars)){
          stop("Missing variable(s): "
              , paste0("'", missing_vars, "'", collapse = ", ")
              , "."
              , call. = FALSE)
       }

       if (missing(weights)){
         weights <- rep(1, length(values))
         names(weights) <- names(values)
       }

       ._value_rules <<- expect_values(values, weights)
       # TODO move this to the outside
       weights <- add_noise(weights)
       objective <<- setNames(weights, paste0(".delta_", names(weights)))
     },
     to_lp = function(...){
       translate_mip_lp(mip_rules(), objective, ...)
     },
     execute = function(...){
       # TODO see if this can be executed in parallel.
       #browser()
       lp <- translate_mip_lp(mip_rules(), objective, ...)
       #TODO set timer, duration etc.
       s <- solve(lp)
       #browser()
       values <- lpSolveAPI::get.variables(lp)
       names(values) <- colnames(lp)
       adapt <- objective < 0 # trick to create logical with names
       adapt_nms <- names(adapt)[names(adapt) %in% names(values)]
       adapt[adapt_nms] <- values[adapt_nms] == 1
       # remove prefix
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
