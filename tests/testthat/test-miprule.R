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
