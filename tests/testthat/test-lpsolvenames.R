describe("LPsolve drops variable names",{
  it("solves an issue",{
    d <- data.frame( id = 101L, a = 1, b = 1, c= 1, d = 1, e = 1
                   , f = 1, s = 4, z = NA_real_
                   )
    rules <- validate::validator(
      s == a + b + c + d + e + f,
      z >= 0
    )

    set.seed(5484)
    el <- errorlocate::locate_errors(d, rules)
    v <- values(el)
    expect_equal(v[1,], c(id=FALSE, a=FALSE, b = FALSE, c = FALSE
                        , d = FALSE, e = FALSE, f = FALSE, s = TRUE, z = NA))
  })
})
