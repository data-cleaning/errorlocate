# a <- c("A:a1"=1)
# mr <- mip_rule(a, "==", 1, "A")
# soft_cat_rule(mr)
#
# x <- c(x=1)
# mr <- mip_rule(x, "<=", 2, "x")
# soft_lin_rule(mr)
#
#
# v1 <- validator(x == 1, y+1==z)
# x <- lin_as_mip_rules(v1)
# ##replace_equal_mip_rules(x)
#
# expect_values(list(x=1, a="A"))
#
context("soft-rule")

describe("soft_lin_rule",{
  it("can transform a lin rule into a softrule",{
    e <- quote(x <= 2)
    mr <- lin_mip_rule_(e, name="x")
    sr <- soft_lin_rule(mr, M = 100)

    expect_equal(sr$a, c(x=1, .delta_x = -100))
    expect_equal(sr$op, "<=")
    expect_equal(sr$rule, "x")
    expect_equal(sr$b, 2)
  })

  it("can transform a cat rule into a softrule",{
    e <- quote(A == 'a')
    mr <- cat_mip_rule_(e, name="A")
    sr <- soft_cat_rule(mr)

    expect_equal(sr$a, c('A:a'=1, .delta_A = 1))
    expect_equal(sr$op, "==")
    expect_equal(sr$b, 1)
    expect_equal(sr$rule, "A")
  })

  it ("can transform a list of num values into softrules", {
    values <- list(x=42)

    sr <- expect_values(values)[[1]]
    expect_equal(sr$a, c(x=1, .delta_x = -1e7))
    expect_equal(sr$op, "<=")
    expect_equal(sr$rule, "x")
    expect_equal(sr$b, 42)

    sr <- expect_values(values)[[2]]
    expect_equal(sr$a, c(x=-1, .delta_x = -1e7))
    expect_equal(sr$op, "<=")
    expect_equal(sr$rule, "x")
    expect_equal(sr$b, -42)
  })

  it ("can transform a list of categorical values into softrules", {
    values <- list(A="a")

    sr <- expect_values(values)[[1]]
    expect_equal(sr$a, c('A:a'=1, .delta_A = 1))
    expect_equal(sr$op, "==")
    expect_equal(sr$b, 1)
    expect_equal(sr$rule, "A")
  })

})
