context("linear")

describe("is_lin_",{
  it("can detect linear expressions",{
    e <- quote(x>1)
    expect_true(is_lin_(e))
  })
  it("can detect non-linear expressions",{
    e <- quote(if(x>1) y < 1)
    expect_false(is_lin_(e))
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
  it("handles unary operators",{
    e <- quote(-x > 1)
    mr <- lin_mip_rule_(e, name="H")
    expect_equal(mr$a, c(x=-1))
    expect_equal(mr$b, 1)
  })
  it("handles permutation of factors",{
    e <- quote(x*2 > 1)
    mr <- lin_mip_rule_(e, name="H")
    expect_equal(mr$a, c(x=2))
    expect_equal(mr$b, 1)
  })
  it("errors on invalid input", {
    e <- quote(if (x < 1) y > 1)
    expect_error(lin_mip_rule_(e))
  })
})

