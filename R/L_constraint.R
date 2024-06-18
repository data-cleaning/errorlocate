#' extract boundary conditions from linear constraints
#' @param x `[OP]` with linear constraints
#' @param types named `character` with types of variables
set_bounds_from_constraints <- function(x){
  is_c <- (x$types == "C")
  is_b <- (x$types == "B")

  # continuous boundaries
  L <- x$constraints$L
  # select only the constraints with one variable
  is_bound = (tabulate(L$i, nbins = L$nrow) == 1)

  idx <- is_bound[L$i]
  # i contains the row numbers of L that are bounds
  i <- L$i[idx]
  # j contains the col numbers / vars
  j <- L$j[idx]
  # contains the sign/coefficient
  v <- L$v[idx]

  d <- c( "<=" =  1
        , "==" =  0
        , ">=" = -1
        )[x$constraints$dir[i]]

  s <- v*d
  # boundary value
  b <- x$constraints$rhs[i] / s

  nobj <- x$n_of_variables

  lb <- ifelse(is_b, 0, -Inf)
  ub <- ifelse(is_b, 1, Inf)

  lb[j[s < 0]] <- b[s < 0]
  li <- which(lb != 0)
  lb <- lb[li]

  ub[j[s > 0]] <- b[s > 0]
  ui <- which(ub < Inf)
  ub <- ub[ui]

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

