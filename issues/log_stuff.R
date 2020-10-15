rules <- validator( log10(prijs) == log10(m2prijs) + log10(oppervlakte)
                  , prijs >= 1e5
                  , oppervlakte >= 20
                  )

d <- data.frame( prijs = 2e5, oppervlakte = 20, m2prijs = 2000)

el <- locate_errors(d, rules, weight = weight)
el$errors


### more detail:

mip <- miprules(rules)

mip$._vars
mip$._vars_num
mip$._log_transform

values <- as.list(d)
weight <-  c(prijs = 1, m2prijs=5, oppervlakte = 5)
log_values <- log_derived_data(values, mip$._log_transform)
mip$set_values( values
                , weights = weight
                , log_values = log_values
                , delta_names = mip$._log_transform$num_vars)
mip$objective
res <- mip$execute()
res
lpSolveAPI::write.lp(res$lp, filename = "test.lp")

