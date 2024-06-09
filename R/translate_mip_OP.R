#' Translates the problem into an ROI::OP mip problem
#'
#' @param rules mip rules
#' @param objective function
#' @param eps accuracy for equality/inequality
#' @param ...additional parameters that are set for the mip problem
translate_mip_OP <- function( rules
                             , objective=NULL
                             , eps = 1e-3
                             , ...
                             ){

  constraints <- get_mr_l_constraint(rules)
  nms <- colnames(constraints$L)
  type <- (get_mr_type(rules))[nms]

  # # TODO improve!
  # lpSolveAPI::lp.control( lps,
  #                         presolve="rows",
  #                         epsint = 1e-15,
  #                         epspivot = 1e-15,
  #                         epsd = 1e-12
  #                       )
  # # overwrite options
  # lpSolveAPI::lp.control( lps, ...)

  # dimnames(lps) <- dimnames(A)
  types = c("double" = "C"
           ,"binary" = "B"
           )[type]
  names(types) <- nms

  if (length(objective)){

    if (is.null(names(objective))){
      names(objective) <- nms
    }
    obj <- objective[nms]

    # set non items to zero
    obj[!is.finite(obj)] <- 0
    obj[obj < 0] <- 0
  } else {
    obj <- rep(0, length(types)) |> setNames(nms)
  }
  op <- ROI::OP( objective   = obj
               , constraints = constraints
               , types = types
               , maximum = FALSE
               )
  op <- set_bounds_from_constraints(op)
  op
}

# splits category names (<variable>:<category>) into variable column groups needed
# for SOS1 constraints
asSOS <- function(vars){
  #TODO also add log lower boundary as SOS
  CAT <- ":.+"

  idx <- grepl(CAT, vars)
  var <- sub(CAT, "", vars)

  sosname <- unique(var[idx])
  sapply(sosname, function(sos){
    columns = which(var == sos)
    list( name=sos
          , columns=columns
          , weights = rep(1, length(columns))
    )
  }, simplify=FALSE)
}

# splits category names (<variable>:<category>) into variable column groups needed
# for SOS1 constraints
asSOS2 <- function(vars){

  #TODO also add log lower boundary as SOS
  LOG <- "(.+\\._x).+"

  idx <- grepl(LOG, vars)
  var <- sub(LOG, "\\1", vars)
  sosname <- unique(var[idx])
  sapply(sosname, function(sos){
    columns = which(var == sos)
    list( name=sos
        , columns=columns
        , weights = rep(1, length(columns))
    )
  }, simplify=FALSE)
}

### testing

# v <- validator( a>1, b+4 > c-z, A %in% "a")
# rules <- lin_as_mip_rules(v)
# translate_mip_lp(c(rules, cat_as_mip_rules(v)))
