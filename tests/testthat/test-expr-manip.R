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
