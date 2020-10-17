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

  it("detects log constants", {
    options(errorlocate.allow_log = TRUE)

    e <- quote(x > log(1))
    expect_true(is_lin_(e))

    options(errorlocate.allow_log = NULL)
  })

  it("detects log transformed variables", {
    options(errorlocate.allow_log = TRUE)

    e <- quote(log(x) > 0)
    expect_true(is_lin_(e))

    options(errorlocate.allow_log = NULL)
  })

})

describe("is_linear",{
  it("can detect linear rules",{
    v <- validator(x > 1, y + 2*x <= 3, A == "a", A == TRUE)
    expect_equal(is_linear(v), c(TRUE, TRUE, FALSE, FALSE))
  })
  it("can detect var_group linear rules",{
    v <- validator(var_group(a,b) >= 0, if (var_group(a,b) == "a") c == TRUE)
    expect_equal(is_linear(v), c(TRUE, FALSE))
  })

  it ("can detect linear rules with log",{
    options(errorlocate.allow_log = TRUE)

    rules <- validator(log(x) > 0, log10(y) > 0, log1p(z) > 0, log(x+y) > 0)
    expect_equal( is_linear(rules)
                , c(TRUE, TRUE, TRUE, FALSE)
                )

    options(errorlocate.allow_log = NULL)
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
    expect_error(lin_mip_rule_(e, name="H"))
  })

  it("evaluates log constants", {
    e <- quote(x > log(1))
    e_e <- quote(x > 0)

    mr <- lin_mip_rule_(e, name="n")
    mr_e <- lin_mip_rule_(e_e, name="n")

    expect_equal(mr, mr_e)
  })

  it("detects log transformed variables", {
    e <- quote(log(x) > 0)
    e_e <- quote(x._log > 0)

    mr <- lin_mip_rule_(e, name="n")
    mr_e <- lin_mip_rule_(e_e, name="n")

    expect_equal(mr, mr_e)
  })


})

