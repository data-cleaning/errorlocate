rules <- validator(x > 1)
data <- list(x = 0)
weight <- c(x = 1)

op <- inspect_OP(data, rules)
print(op)

if (ROI::ROI_require_solver("highs")){
  sol <- ROI::ROI_solve(op, "highs")
  print(sol)
  ROI::solution(sol)
}
