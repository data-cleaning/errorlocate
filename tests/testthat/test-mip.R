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
    res <- mr$execute()
    expect_equal(res$adapt, c(x=TRUE))
  })
  it("can handle strict inequalities",{
    rules <- validator(if ( x > 0 ) y > 0)
    data <-  data.frame( x = 1
                       , y = 0
                       )

    mr <- miprules(rules)
    mr$set_values(data)
  })

})
