register_ROI_solvers <- function(){
  inst <- ROI::ROI_installed_solvers()
  for (solver in names(inst)){
    ROI::ROI_require_solver(solver = solver)
  }
}
