rules <- validator( log(prijs) == log(m2prijs) + log(oppervlakte)
                  , prijs >= 1e5
                  , oppervlakte >= 20
                  )

d <- data.frame( prijs = 2e5, oppervlakte = 20, m2prijs = 2000)

weight <-  c(prijs = 1, m2prijs=5, oppervlakte = 5)
el <- locate_errors(d, rules, weight = weight)
el$errors


### more detail:

mip <- inspect_mip(d, rules, weight)
mip$objective
res <- mip$execute()
res
lpSolveAPI::write.lp(res$lp, filename = "test.lp")
