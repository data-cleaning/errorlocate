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
  it("can handle empty values",{
    rules <- validator(if ( x > 0 ) y > 0)
    mr <- miprules(rules)
    mr$set_values()
    mr$set_values(NULL)
    mr$set_values(list())
  })
  it("tests for infeasibility",{
    rules <- validator(x > 1)
    mr <- miprules(rules)
    expect_equal(mr$is_infeasible(), FALSE)

    rules <- validator(x > 1, x < 1)
    mr <- miprules(rules)
    expect_equal(mr$is_infeasible(), TRUE)
  })
  it("prints", {
    rules <- validator(if ( x > 0 ) y > 0)
    data <-  data.frame( x = 1
                       , y = 0
    )
    mr <- miprules(rules)
    mr$set_values(data)
    expect_output(print(mr))
  })
  it("warns for ignoring rules", {
    rules <- validator( x > 1
                      , y > mean(x)
                      , z < min(x)
                      , y > 0
                      )
    expect_warning(mr <- miprules(rules), "Ignoring rules")
    expect_equivalent(mr$._ignored$exprs(), rules[2:3]$exprs())
  })
})
