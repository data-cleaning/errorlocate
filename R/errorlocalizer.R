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
        # maybe move this to function arguments.
        show_progressbar <- interactive()

        vars <- ._miprules$._vars
        nr_rows <- nrow(data)
        nr_cols <- ncol(data)
        names_cols <- colnames(data)

        missing_vars <- vars[!vars %in% names(data)]

        if (length(missing_vars)){
          # if they are part of the environment remove...
          mv_in_env <- sapply(missing_vars, exists)
          vars <- setdiff(vars, missing_vars[mv_in_env])
          ._miprules$._vars <<- vars

          missing_vars <- missing_vars[!mv_in_env]
        }

        if (length(missing_vars)){
          stop('Missing column(s): '
              , paste0("'", missing_vars, "'", collapse = ", ")
              , ". Add them to your data and rerun."
              , call. = FALSE
              )
        }
        #browser()
        numvars_mip <- ._miprules$._vars_num
        numvars_data <- names(data)[sapply(data, is.numeric)]
        categorical_as_integer <- numvars_data[ numvars_data %in% vars
                                              & !numvars_data %in% numvars_mip
                                              ]
        if (length(categorical_as_integer)){
          stop('Categorical columns coded as integers: '
               , paste0("'", categorical_as_integer, "'", collapse = ", ")
               , ". Change them to `factor` and rerun."
               , call. = FALSE
          )
        }

        if (length(weight) == 0){
          weight <- matrix(1, nrow=nr_rows, ncol=nr_cols)
          colnames(weight) <- names_cols
        } else {
          if (is.null(dim(weight))){
            if (length(weight) == ncol(data)){

              if (!is.null(names(weight))){
                weight <- weight[names_cols]
              }
              # use recycling to fill a weight matrix and transpose...
              weight <- t(matrix(weight, nrow=nr_cols, ncol=nr_rows))
              colnames(weight) <- names_cols
            }
          }
          stopifnot(dim(weight) == dim(data))
          if (is.null(colnames(weight))){
            colnames(weight) <- names_cols
          } else {
            weight <- weight[, names_cols, drop=FALSE]
          }
          stopifnot(names(weight) == names_cols)
        }

        # derive log transformed data!
        log_transform <- ._miprules$._log_transform
        # TODO deal with failures when log of negative values is taken...
        log_data <- log_derived_data(data, log_transform)

        # TODO add `n` to arguments of function
        # set ranges of log constraints if any
        ._miprules$update_log_constraints(data, n = 10)

        N <- nr_rows
        rows <- seq_len(N)


        # filter for records that are valid..., that reduces the processing
        # time considerably
        cf <- validate::confront(data, rules)
        invalid <- aggregate(cf, by = "record")$nfail > 0

        n_invalid <- sum(invalid)

        if (n_invalid == 0){
          show_progressbar <- FALSE
        }

        if (show_progressbar) {
          pb <- utils::txtProgressBar(min = 0, max=n_invalid, style = 3)
        }

        #TODO add ref data !!!

        # seems strange, but we are going to transpose..
        res <- matrix( FALSE
                     , nrow = nr_cols
                     , ncol = nr_rows
                     , dimnames = list(names_cols)
                     )


        # collect info during processing
        status <- integer(N)
        duration <- numeric(N)
        solution <- logical(N)
        solution[] <- TRUE

        # TODO remove any(invalid)
        if (n_invalid > 0){
          for (r in rows[invalid]){
            starttime <- Sys.time()
            values <- as.list(data[r,,drop=FALSE])
            ._miprules$set_values( values = values
                                 , weight[r,]
                                 , log_values = as.list(log_data[r,,drop=FALSE])
                                 , delta_names <- log_transform$num_vars
                                 )
            el <- ._miprules$execute(timeout=timeout, ...)
            adapt <- sapply(values, function(x){FALSE})
            adapt[names(el$adapt)] <- el$adapt
            status[r] <- el$s
            solution[r] <- el$solution
            rm(el)
            gc()
            if (show_progressbar){
              value <- 1 + pb$getVal()
              utils::setTxtProgressBar(pb, value)
            }
            duration[r] <- Sys.time() - starttime
            res[, r] <- adapt
          }
        }
        if(show_progressbar){ close(pb) }

        adapt <- t(res)
        idx <- which(colnames(adapt) %in% colnames(weight))
        weight_per_record <- as.numeric(tcrossprod(adapt[,idx], weight))

        if (any(!solution)){
          warning("For some records the procedure was unsuccesful, "
                 , "please check the '$solution' and '$status' of the errorlocations.\n"
                 , "Records: "
                 , paste0(which(!solution), collapse = " ,")
                 , call. = FALSE
                 )
        }

        is.na(adapt) <- is.na(data)
        create_errorlocation(
          values = adapt,
          weight = weight_per_record,
          duration = duration,
          status = status,
          solution = solution
        )
      }
    )
)

