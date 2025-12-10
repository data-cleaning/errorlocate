#' Create a mip object from a validator object
#'
#' Create a mip object from [validate::validator()] object.
#' This is a utility class that translates a validor object into a mixed integer problem that
#' can be solved.
#' Most users should use [locate_errors()] which will handle all translation and execution
#' automatically. This class is provided so users can implement or derive an alternative solution.
#'
#' @section Methods:
#' The `MipRules` class contains the following methods:
#' \itemize{
#'   \item `$execute()` calls the mip solver to execute the rules.
#'   \item `$to_lp()`: transforms the object into a lp_solve object
#'   \item `$is_infeasible` Checks if the current system of mixed integer rules is feasible.
#'   \item `$set_values`: set values and weights for variables (determines the objective function).
#' }
#'
#' @family Mixed Integer Problem
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
     ._miprules    = "list",
     ._log_rules   = "list", # extra constraints for log transformed rules
     ._value_rules = "list",
     ._vars        = "character",
     ._vars_num    = "character",
     ._log_transform = "data.frame",
     ._ignored     = "ANY",
     ._lp          = "ANY",
     ._values      = "ANY"
   ),
   methods = list(
     initialize = function(rules = NULL, n = 10){
       if (is.null(rules)){ return()}
       rules <<- rules
       objective <<- objective

       ._miprules <<- to_miprules(rules)
       ._vars <<- get_translatable_vars(rules)

       var_num <- sapply(._miprules, function(mr){
          names(mr$type)[mr$type == "double"]
       })
       var_num <- as.character(unique(unlist(var_num)))

       # extract log transformed variables
       ._log_transform <<- log_extract(var_num)

       # set log constraints
       ._log_rules <<- create_log_constraints(._log_transform)

       # make sure original variables are also in _vars_num
       ._vars_num <<- unique(c(._log_transform$num_vars, var_num))

     },
     mip_rules = function(){
       c(._miprules, ._log_rules, ._value_rules)
     },
     set_values = function( values
                          , weights
                          #, log_values = log_derived_data(values, ._log_transform)
                          #, delta_names= ._log_transform$num_vars)
                          ){
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

       # omitting vars that are not in rules...
       values <- values[._vars]
       ._values <<- values
       weights <- weights[._vars]

       # TODO if missing log_values, derive it inplace

       ._value_rules <<- expect_values(values, weights)
       # if (length(log_values)){
       #    weights_ld <- weights[delta_names]
       #    names(weights_ld) <- names(log_values)
       #    names(delta_names) <- names(log_values)
       #    log_value_rules <- expect_values(log_values, weights = weights_ld, delta_names)
       #    ._value_rules <<- c(._value_rules, log_value_rules)
       # }

       # TODO move this to the outside
       weights <- add_noise(weights)
       objective <<- setNames(weights, paste0(".delta_", names(weights)))
     },
     update_log_constraints = function(data, n = 10){
       ._log_rules <<- create_log_constraints(._log_transform
                                             , data = data
                                             , n = n
                                             )
     },
     to_lp = function(...){
       translate_mip_lp(mip_rules(), objective, ...)
     },
     write_lp = function(filename, ...){
        lpSolveAPI::write.lp(to_lp(), filename, ...)
     },
     execute = function(...){
       lp <- translate_mip_lp(mip_rules(), objective, ...)
       #TODO set timer, duration etc.
       s <- solve(lp)

       solution <- switch( as.character(s),
                           "0" = TRUE,  # optimal solution found (so feasible)
                           "1" = TRUE,  # sub optimal solution (so feasible)
                           "2" = FALSE, # infeasible
                           "3" = TRUE,  # unbounded (so feasible)
                           "4" = TRUE,  # degenerate (so feasible)
                           # "5" = NA,    # numerical failure, so unknown
                           # "6" = NA,    # process aborted
                           # "7" = NA,    # timeout
                           "9" = TRUE,  # presolved
                           "10" = FALSE, # branch and bound failed
                           "11" = FALSE, # branch and bound stopped
                           "12" = TRUE,  # a feasible branch and bound found
                           "13" = FALSE, # no feasible branch and bound found
                           FALSE
       )
       if (isTRUE(solution)){
          values <- lpSolveAPI::get.variables(lp)
       } else {
          values <- rep(1, ncol(lp))
       }
       names(values) <- colnames(lp)
       # browser()

       adapt <- objective[is.finite(objective)] < 0  # trick to create logical with names
       adapt_nms <- names(adapt)[names(adapt) %in% names(values)]
       adapt[adapt_nms] <- values[adapt_nms] == 1
       # issue with lpsolve: it sometimes messes up column names...
       vm <- !names(adapt) %in% names(values)

       if (length(values) == 0){
          # seems optimalisation of lpSolvAPI when there is only 1 column of data..
          # adapt <- objective > 0
          adapt[] <- lpSolveAPI::get.objective(lp) > 0
          #browser()
       }

       # remove prefix
       names(adapt) <- gsub(".delta_", "", names(adapt))
       if (any(vm)){
         vm <- names(adapt)[vm]
         vm <- vm[vm %in% names(values)]
         # print(vm)
         adapt[vm] <- sapply(vm, function(nm){
           !isTRUE(unname(values[nm]) == ._values[[nm]])
         })
                }
       # TODO improve the return values based on value of s
       # Add the same table as with infeasible

       adapt <- unlist(adapt)
       list(
         s = s,
         solution = solution,
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

       lp <- translate_mip_lp( mr
                             , obj
                             , break.at.first = TRUE
                             )
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
       cat("Mip rules object:\n")
       cat("   methods: '$to_lp()', '$execute', '$set_values()'\n")
       cat("   properties: '$mip_rules', '$objective', '$is_infeasible', '$rules'\n")
       # print(mr)
       cat("\n")
       cat("Generates the lp program (see ?inspect_mip) \n\n")
       print(to_lp())
       # cat(paste("* " , sapply(head(mr), as.character.mip_rule), collapse = "\n"))
       # if (length(mr) > 6){
       #   cat("\n...\nTotal of ", length(mr), " rules")
       # }
     }
   )
)


### testing
# v <- validator(x>3, y>2)
# m <- mip(v)
