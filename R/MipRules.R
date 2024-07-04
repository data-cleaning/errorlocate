#' Create a mip object from a validator object
#'
#' Create a mip object from [validator()] object.
#' This is a utility class that translates a validor object into a mixed integer problem that
#' can be solved.
#' Most users should use [locate_errors()] which will handle all translation and execution
#' automatically. This class is provided so users can implement or derive an alternative solution.
#'
#' @section Methods:
#' The `MipRules` class contains the following methods:
#' \itemize{
#'   \item `$execute()` calls the mip solver to execute the rules.
#'   \item `$to_OP()`: transforms the object into a [ROI::OP()] object
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
     ._values      = "ANY",
     ._solver_config      = "list"
   ),
   methods = list(
     initialize = function( rules = NULL
                          , solver_config = get_solver_config()
                          , n = 10){
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

       ._solver_config <<- solver_config

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
     to_OP = function(...){
       translate_mip_OP(mip_rules(), objective, ...)
     },
     execute = function(...){
       op <- translate_mip_OP(mip_rules(), objective, ...)
       tryCatch({
         s <- ROI::ROI_solve(
           op,
           solver = ._solver_config$solver,
           control = ._solver_config$control,
           ...
        )
      },
      error = function(e){
        warning(e$message, call. = FALSE)
        s <- list(
          status = list(
            code  = 1,
            msg = list(e)
          )
        )
      }
      )

      if (s$status$code == 0){
        values <- s$solution
      } else {
        values <- sapply(op$objective$names, function(n) {1})
      }
      adapt <- objective[is.finite(objective)] < 0  # trick to create logical with names
      adapt_nms <- names(adapt)[names(adapt) %in% names(values)]
       adapt[adapt_nms] <- values[adapt_nms] == 1
       # issue with lpsolve: it sometimes messes up column names...
       vm <- !names(adapt) %in% names(values)

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
         s = s$status$code,
         solver_msg = s$status$msg,
         solution = (s$status$code == 0),
         values = values,
         op = op,
         adapt = adapt,
         errors = adapt
       )
     },

     is_infeasible = function(){  # since we only check a subset of the rules,
       mr <- .self$mip_rules()    # we can only detect infeasiblity, not feasiblity
       vars <- get_mr_vars(mr)

       obj <- rep(1, length(vars))
       names(obj) <- vars

       op <- translate_mip_OP( mr
                             , obj
                             )

       tryCatch({
         s <- ROI::ROI_solve(
           op,
           solver = ._solver_config$solver,
           control = ._solver_config$control
         )
         return(s$status$code != 0)
         },
         error = function(e){
           return(TRUE)
         }
       )
     },
     show = function(){
       mr <- mip_rules()
       cat("Mip rules object:\n")
       cat("   methods: '$to_OP()', '$execute', '$set_values()'\n")
       cat("   properties: '$mip_rules', '$objective', '$is_infeasible', '$rules'\n")
       cat("\n")
       cat("Generates the lp program (see ?inspect_mip) \n\n")
       print_OP(to_OP())
     }
   )
)


### testing
# v <- validator(x>3, y>2)
# m <- mip(v)
