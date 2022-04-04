# context("expressions")
#
# describe("contains",{
#   it("should find operators",{
#     e <- quote(x < 1)
#     expect_false(contains_string_(e))
#     expect_true(contains_op_(e, '<'))
#   })
#   it("should find strings",{
#     e <- quote(x == "A")
#     expect_true(contains_string_(e))
#     expect_true(contains_op_(e,"=="))
#     expect_true(contains_value_(e, 'A'))
#   })
# })


describe("rewrite_ratio", {
  res <- rewrite_ratio(quote(a / b <= 1))
  expect_equal(res, quote(a <= b))

  res <- rewrite_ratio(quote(a / b == 1))
  expect_equal(res, quote(a == b))

  res <- rewrite_ratio(quote(a / b >= 1))
  expect_equal(res, quote(a >= b))

  res <- rewrite_ratio(quote((a+1) / b >= 1))
  expect_equal(res, quote(a +1 >= b))

  res <- rewrite_ratio(quote(a / (b + 1) >= 1))
  expect_equal(res, quote(a  >= b + 1))

  res <- rewrite_ratio(quote((2*a+2) / (3*b + 1) >= 1))
  expect_equal(res, quote(2*a + 2 >= 3*b + 1))

})

describe("rewrite in_range", {
  it("checks if num range", {
    e <- quote(in_range(x, 2, 5))
    expect_true(is_num_range(e))

    e <- quote(in_range(x, "a", "b"))
    expect_false(is_num_range(e))
  })

  it("rewrites in_range", {
    e <- quote(in_range(x, 2, 5))
    e2 <- rewrite_in_range(e)
    expect_equal(e2, quote((x >= 2) & (x <= 5)))

    e <- quote(in_range(x, 2*y, 5*y))
    e2 <- rewrite_in_range(e)
    expect_equal(e2, quote((x >= 2*y) & (x <= 5*y)))

  })
})
