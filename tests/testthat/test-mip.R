context("mip")

describe("MipRules",{
  it("can transform to miprules", {
    v <- validator(x > 1)
    mr <- miprules(v)
  })

  it("can set values", {
    v <- validator(x > 1)
    mr <- miprules(v)
    mr$set_values(list(x=1))
    expect_equal(length(mr$mip_rules()), 3)
  })

  it("finds errors", {
    v <- validator(x > 1)
    mr <- miprules(v)
    mr$set_values(list(x=0.1))
    mr$execute()      })

})
