context("lpsolve")

describe("lp_solve",{
  it("can translate simple lin rule", {
    v <- validator(3*x <= 2)

    lin_rules <- lin_as_mip_rules(v)
    exp <- list( a = 3,
                 op = "<=",
                 b = 2,
                 rule = "V1",
                 type = "double",
                 weight = Inf)

    expect_equivalent(lin_rules[[1]], exp)
  })
  it("can translate lin rules to lpsolve object", {
    v <- validator(x > 1, y < 3)
    lin_rules <- lin_as_mip_rules(v)
    translate_mip_lp(lin_rules)
  })
})
