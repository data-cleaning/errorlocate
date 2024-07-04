get_solver_config <- function(solver, control = list()){
  if (missing(solver)){
    solver <- getOption("errorlocate.solver", "lpsolve")
  }
  ROI::ROI_require_solver(solver)

  # determine setting for a certain solver
  control <- switch(
    solver,

    lpsolve = list(
      presolve="rows",
      epsint = 1e-15,
      epspivot = 1e-15,
      epsd = 1e-12
    ),

    highs = list(
      presolve = "off",
      mip_feasibility_tolerance = 1e-10
    ),

    {
      list()
    }
  )

  list(
    solver = solver,
    control = control
  )
}
