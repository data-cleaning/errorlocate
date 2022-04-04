context("miprule")

describe("get_mr_type",{
  it("can return correct type",{
    v <- validator(x > 1, A %in% 'b', x + y==3)
    rules <- c(cat_as_mip_rules(v), lin_as_mip_rules(v))
    expect_equal(get_mr_type(rules), c("A:b"="binary", x="double", y="double"))
  })
  it("can print", {
    v <- validator(x > 1, A %in% 'b', x + y==3)
    rules <- c(cat_as_mip_rules(v), lin_as_mip_rules(v))
    expect_output(print(rules))
  })
  it("can be an expression",{
    rules <- validator( x > 1
                      , A %in% 'b'
                      , x + y==3
                      )
    #mr <- miprules(rules)$mip_rules()
    #get_mr_expression(mr)
    #get_mr_weights(mr)
  })
})

describe("print mr rule", {
  rules <- validator( x  < 1.1*y, 2*x  < y)
  mr <- to_miprules(rules)
  expect_output(print(mr[[1]]), "V1: x - 1.1*y < 0", fixed = TRUE)
  expect_output(print(mr[[2]]), "V2: 2*x - y < 0", fixed = TRUE)
})

describe("print mr rule", {
  rules <- validator( in_range(age, 18, 67))
  mr <- to_miprules(rules)

  expect_output(print(mr[[1]]), "V1: -age <= -18", fixed = TRUE)
  expect_output(print(mr[[2]]), "V1: age <= 67", fixed = TRUE)
})
