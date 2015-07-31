context("expressions")

describe("contains",{
  it("should find operators",{
    e <- quote(x < 1)
    expect_false(contains_string_(e))
    expect_true(contains_op_(e, '<'))
  })
  it("should find strings",{
    e <- quote(x == "A")
    expect_true(contains_string_(e))
    expect_true(contains_op_(e,"=="))
    expect_true(contains_value_(e, 'A'))
  })
})
