options(errorlocate.allow_log = TRUE)

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

res$values

rules <- validator(log(x) > log(y), x < y)
data <- data.frame(x = 1e3, y = 2e3)
mip <- inspect_mip(data, rules)
res <- mip$execute()
res

res <- sapply(10:80, function(n){
  mip <- inspect_mip(data, rules, n = n)
  mip$execute()$s
})

