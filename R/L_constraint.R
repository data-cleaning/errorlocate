#' extract boundary conditions from linear constraints
#' @param x `[OP]` with linear constraints
#' @param types named `character` with types of variables
set_bounds_from_constraints <- function(x){
  is_c <- (x$types == "C")
  is_b <- which(x$types == "B")

  # continous boundaries
  L <- x$constraints$L
  A <- abs(L)
  is_bound = (apply(A, 1, sum) == 1)
  s <- sign(apply(L, 1, sum))

  d <- c( "<=" =  1
        , "==" =  0
        , ">=" = -1
        )[x$constraints$dir]

  s <- s*d

  nobj <- x$n_of_variables

  li <- seq_len(nobj)
  lb <- rep(-Inf, nobj)

  i <- is_bound & (s <= 0)
  lic <- apply(A[i,], 1, function(x){which(is_c & (x > 0))})
  lb[lic] <- x$constraints$rhs[i]
  lb[is_b] <- 0


  ui <- seq_len(nobj)
  ub <- rep(Inf, nobj)

  i <- is_bound & (s >= 0)
  uic <- apply(A[i,], 1, function(x){which(is_c & (x > 0))})
  ub[uic] <- x$constraints$rhs[i]
  ub[is_b] <- 1L


  vb <- ROI::V_bound(
    li = li,
    ui = ui,
    lb = lb,
    ub = ub,
    nobj = nobj
  )

  ROI::bounds(x) <- vb

  x
}
