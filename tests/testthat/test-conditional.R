library(testthat)
context("conditional rules")

describe("is_condition_",{
  it("can detect a complex rule", {
    e <- quote(y == 1 | z <= 1)
    expect_true(is_condition_(e))
  })
})

describe("conditional", {
  it("can detect conditional rules", {
    v <- validator( if ( x > 1) y == 1,
                    if (x > 1) a == "a1",
                    a %in% c("a1", "a2"), # pure categorical
                    if (A=="a") B == "b", # pure categorical
                    x > 1                 # pure linear
    )
    expect_equal( is_conditional(v)
                , c(TRUE, TRUE, FALSE, FALSE, FALSE))
  })
  it("can detect more complex rules", {
    v <- validator( if ( x > 1 && z >= 1) y == 1,
                    if (x > 1) y == 1 || z <= 1,
                    if (x > 1) y == 1 | z <= 1,
                    y == 1 | z <= 1
    )
    expect_equal( is_conditional(v)
                  , c(TRUE, TRUE, TRUE, TRUE))
  })
})
