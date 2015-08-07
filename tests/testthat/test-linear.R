context("linear")

describe("is_lin_",{
  it("can detect linear expressions",{
    e <- quote(x>1)
    expect_true(is_lin_(e))
  })
})

describe("is_linear",{
  it("can detect linear rules",{
    v <- validator(x > 1, y + 2*x <= 3, A == "a", A == TRUE)
    expect_equal(is_linear(v), c(TRUE, TRUE, FALSE, FALSE))
  })
})

describe("lin_mip_rule",{
  it("can create a linear mip_rule object",{
    e <- quote(x > 1)
    mr <- lin_mip_rule_(e, name="H")
    expect_equal(mr$a, c(x=1))
    expect_equal(mr$b, 1)
  })
})

