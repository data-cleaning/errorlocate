# provides an interface to mip solvers.
# currently only lpSolveAPI, should be workable for glpt

#' translate linear rules into an lp problem
#' @importFrom lpSolveAPI dimnames<-.lpExtPtr
#' @param  rules mip rules
#' @param objective function
#' @param eps accuracy for equality/inequality
#' @param ... additional [lp.control()] parameters that are set for the mip problem
translate_mip_lp <- function( rules
                            , objective=NULL
                            , eps = 1e-3
                            , ...
                            ){
  #browser()
  lc <- get_mr_matrix(rules)
  type <- get_mr_type(rules)

  A <- lc$A
  nvar <- ncol(A)

  lps <- lpSolveAPI::make.lp( nrow = nrow(A)
                            , ncol = nvar
                            )

  lpSolveAPI::name.lp(lps, "errorlocate")
  # TODO improve!
  lpSolveAPI::lp.control( lps,
                          presolve = c(
                              "rows"
                            , "cols"
                            #, "probefix"
                          ),
                          epsint = 1e-15,
                          epspivot = 1e-15,
                          ...
                        )

  dimnames(lps) <- dimnames(A)

  for (v in 1:ncol(A)){
    lpSolveAPI::set.column(lps, v, A[,v])
  }

  ops <- lc$operator
  ops[ops=="=="] <- "="
  ops[strict <- ops=="<"] <- "<="

  is_binary <- type  == "binary"
  if (any(is_binary)){
    columns <- type[is_binary]
    columns <- match(names(columns), colnames(A))
    lpSolveAPI::set.type(lps, columns, "binary")
  }
  is_double <- !is_binary

  if (any(is_double)){
    columns <- type[is_double]
    columns <- match(names(columns), colnames(A))
    lpSolveAPI::set.type(lps, columns, "real")
    lpSolveAPI::set.bounds(lps, lower=rep(-Inf, length(columns)), columns=columns)
  }

  # should improve performance quite a lot: a SOS1 makes bin variables exclusive.
  for (sos in asSOS(colnames(lps))){
    lpSolveAPI::add.SOS( lps, sos$name,
             type=1, priority=1,
             columns=sos$columns,
             weights=sos$weights
    )
  }

  # should improve performance quite a lot: a SOS1 makes bin variables exclusive.
  for (sos2 in asSOS2(colnames(lps))){
    lpSolveAPI::set.bounds(lps
                          , lower=rep(0L, length(sos2$columns))
                          , upper=rep(1L, length(sos2$columns))
                          , columns=sos2$columns
                          )

    lpSolveAPI::add.SOS( lps, sos2$name,
                         type=2, priority = 1,
                         columns=sos2$columns,
                         weights = sos2$weights
    )

  }

  if (length(objective)){
    obj <- objective[objective != 0]
    columns <- match(names(obj), colnames(A))

    # this is due to NA variables that are not part of the problem any more...

    if (any(is_na <- is.na(columns))){
      obj <- obj[!is_na]
      columns <- columns[!is_na]
      #stop("Invalid objective function")
    }
    lpSolveAPI::set.objfn(lps, unname(obj), columns)
  }
  #browser()
  lpSolveAPI::set.constr.type(lps,types=ops)

  b <- ifelse(strict, lc$b - eps, lc$b)
  lpSolveAPI::set.constr.value(lps, b)
#  print(colnames(lps))
  lps
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
