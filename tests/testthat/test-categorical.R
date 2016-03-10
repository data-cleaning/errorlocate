library(testthat)
context("categorical rules")

describe("categorical", {
  it("can detect categorical rules", {
    v <- validator( a %in% c("a1", "a2"),
                    if (a %in% 'a1') b == "b1",
                    x > 1,
                    if (x>1) a == "a1"
                  )
    expect_equal(is_categorical(v), c(TRUE, TRUE, FALSE, FALSE))
  })
  it("can derive coefficients",{
    v <- validator( a %in% c("a1", "a2"),
                    if (a %in% 'a1') b == "b1"
    )
    coef <- cat_coefficients(v)
    expect_equal( coef$A
               ,  matrix( c(1, 1, 1, 0, 0, -1)
                        , ncol = 3
                        , dimnames = list(
                            rule = c("V1", "V2"),
                            variable = c("a:a1", "a:a2", "b:b1")
                        )))
    expect_that(coef$operator, equals(c("==", "<=")))
    expect_that(coef$b, equals(c(1, 0)))
  })

  it("can handle logical rules", {
    v <- validator( married %in% c(TRUE, FALSE)
                  , adult %in% c(TRUE, FALSE)
                  , if (married==TRUE) adult==TRUE
                  )
    cat_coefficients(v)
  })
})
