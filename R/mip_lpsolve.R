# provides an interface to mip solvers.
# currently only lpSolveAPI

#' translate linear rules into an lp problem
#' @importFrom lpSolveAPI dimnames<-.lpExtPtr
#'
translate_mip_lp <- function( rules
                        , objective=NULL
                        , eps = 1e-7
                        , bin_vars = integer()
                        ){

  lc <- get_mr_matrix(rules)

  A <- lc$A
  nvar <- ncol(A)

  lps <- lpSolveAPI::make.lp( nrow = nrow(A)
                            , ncol = nvar
                            )

  # ugly....
  lpSolveAPI::`dimnames<-.lpExtPtr`(lps, dimnames(A))
  dimnames(lps) <- dimnames(A)

  for (v in 1:ncol(A)){
    lpSolveAPI::set.column(lps, v, A[,v])
  }

  ops <- lc$operator
  ops[ops=="=="] <- "="
  ops[strict <- ops=="<"] <- "<="

  lpSolveAPI::set.constr.type(lps,types=ops)

  if (!is.null(objective)){
    lpSolveAPI::set.objfn(lps, objective)
  }

  lpSolveAPI::set.bounds(lps, lower=rep(-Inf, ncol(A)))
  b <- ifelse(strict, lc$b - eps, lc$b)
  lpSolveAPI::set.constr.value(lps, b)
  lps
}

### testing

# v <- validator( a>1, b+4 > c-z)
# rules <- lin_as_mip_rules(v)
# translate_mip_lp(rules)
