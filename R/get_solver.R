get_solver <- function(){
  solver <- getOption("errorlocate.solver", "lpsolve")
  ROI::ROI_require_solver(solver)
  solver
}
