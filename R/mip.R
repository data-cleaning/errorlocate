#' Write rules into a mip representation
#'
#' Writes a rules object into a mip problem.
#' @param E an \code{link{editset}} or an object that is coerciable to an
#' \code{editset}
#' @param x named \code{list}/\code{vector} with variable values
#' @param weight reliability weights for values of \code{x}
#' @param M Constant that is used for allowing the values to differ from \code{x}
#' @param epsilon Constant that is used for converting '<' into '<='
#' @param prefix prefix for dummy variables that are created
#' @param ... not used
#' @return a mip object containing al information for transforming it
#' into an lp/mip problem
#' @export
as.mip <- function( E, x=NULL, weight=NULL, M=1e7, epsilon=1e-3, prefix="delta."
                    , ...){
  structure(
    list( E = E_mip
          , objfn = objfn
          , binvars = which(binvars)
          , numvars = which(numvars)
          , M = M
          , epsilon = epsilon
    ),
    class="mip"
  )
}

#' @method print mip
print.mip <- function(x, ...){
}

# # quick test
# E <- editset(c(r1="x > 1","y >= x", r2="if (x>1) y> 2", r3="A %in% c('a', 'b')"))
# as.mip(E)
