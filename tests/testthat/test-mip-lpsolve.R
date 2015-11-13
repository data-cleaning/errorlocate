context("lpsolve")

describe("lp_solve",{
  it("can translate lin rules to lpsolve object", {
    v <- validator(x > 1, y < 3)
    lin_rules <- lin_as_mip_rules(v)
    translate_mip_lp(lin_rules)
  })
})
