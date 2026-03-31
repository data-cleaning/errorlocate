rules <- validator(x > 1)
data <- list(x = 0)
weight <- c(x = 1)

mip <- inspect_mip(data, rules)
print(mip)

# inspect the LP problem (prior to solving it with lpSolveAPI)
lp <- mip$to_lp()
print(lp)

# for large problems write the LP problem to disk for inspection
# lpSolveAPI::write.lp(lp, "my_problem.lp")

# solve the MIP system / find a solution
res <- mip$execute()
names(res)

# lpSolveAPI status of finding a solution
res$s

# LP problem after solving (often simplified version of first LP)
res$lp

# records that are deemed "faulty"
res$errors

# values of variables used in the MIP formulation. Also contains a valid solution
# for "faulty" variables
res$values

# see the derived MIP rules and objective function, used in the construction of
# the LP problem
mip$mip_rules()
mip$objective
