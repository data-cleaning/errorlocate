# provides an interface to mip solvers.
# currently only lpSolveAPI

NULL

#' translate linear rules into an lp problem
translate_lp <- function( rules
                        , objective=NULL
                        , eps = 1e-7
                        , bin_vars = integer()
                        ){
  lc <- rules$linear_coefficients()

  A <- lc$A
  nvar <- ncol(A)
  lps <- lpSolveAPI::make.lp( nrow = nrow(A)
                            , ncol = nvar
                            )

  dimnames(lps) <- dimnames(A)

  for (v in 1:ncol(A)){
    lpSolveAPI::set.column(lps, v, A[,v])
  }

  ops <- lc$operators
  ops[ops=="=="] <- "="
  ops[strict <- ops=="<"] <- "<="

  lpSolveAPI::set.constr.type(lps,types=ops)

  if (!is.null(objective)){
    lpSolveAPI::set.objfn(lps, objective)
  }

  lpSolveAPI::set.bounds(lps, lower=rep(-Inf), columns=)

  b <- ifelse(strict, lc$b - eps, lc$b)
  lpSolveAPI::set.constr.value(lps, b)
  lps
}


### testing
# v <- validator(a>1)
# translate_lp(v)
