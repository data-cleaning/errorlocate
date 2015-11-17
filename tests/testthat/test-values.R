context("values")

describe("substitute values", {
  it("can fill in values", {
    v <- validator(x > 1)
    res <- substitute_values(v, list(x=2))
    expect_equivalent(list(), res)
  })
})
