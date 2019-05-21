context("locate_errors")

describe("locate_errors", {
  it("works with validator",{
    v <- validator(x > 1)
    dat <- data.frame(x = c(2, 0))
    loc <- fh_localizer(v)
    inherits(loc, "ErrorLocalizer")
    locate_errors(dat, loc)
  })
  it("works with linear rules",{
    v <- validator( profit + cost == turnover
                    , cost - 0.6*turnover >= 0
                    , cost>= 0
                    , profit >= 0
    )
    data <- data.frame(profit=755, cost=125, turnover=200)
    le <- locate_errors(data, v)
    expect_equivalent(values(le)[1,], c(profit=TRUE, cost=FALSE, turnover=FALSE))
  })
  it("works with categorical rules",{
    v_categorical <- validator( A %in% c("a1", "a2")
                              , B %in% c("b1", "b2")
                              ,  if (A == "a1") B == "b1"
    )

    set.seed(42) # because of random noise added to weight
    data <- data.frame(A = c("a1", "a2"), B = c("b2", "b2"))
    le <- locate_errors(data, v_categorical)
    expect_equivalent( values(le)
                     , matrix( c( TRUE, FALSE
                                , FALSE, FALSE
                                )
                                , nrow=2, dimnames = list(NULL, c("A", "B"))
                             )
                     )
  })

  it("works with logical variables",{
    v_logical <- validator( A %in% c(TRUE, FALSE)
                            , B %in% c(TRUE, FALSE)
                            ,  if (A == TRUE) B == TRUE
    )

    data <- data.frame(A = TRUE, B = FALSE)
    le <- locate_errors(data, v_logical, weight=c(A=2,B=1))
    expect_equivalent(values(le)[1,], c(A=FALSE, B=TRUE))
  })

  it("works with conditional rules",{
    v <- validator( married %in% c(TRUE, FALSE), if (married==TRUE) age >= 17 )
    data <- data.frame( married = TRUE, age = 16)
    le <- locate_errors(data, v, weight=c(married=1, age=2))
    expect_equivalent(values(le)[1,], c(married=TRUE, age = FALSE))
  })

  it("handles NA values in categorical rules",{
    rules <- validator(a %in% c("A","B"))
    data <- data.frame(a=NA_character_)
    le <- locate_errors(data, rules)
    errors <- values(le)
    expect_equivalent(errors[1,], c(a = NA))
  })

  it("handles Inf weights",{
    skip("Enhancement")
    v <- validator( profit + cost == turnover
                  , cost - 0.6*turnover >= 0
                  , cost>= 0
                  , turnover >= 0
    )
    data <- data.frame(profit=100, cost=125, turnover=200)
    le <- locate_errors(data, v, weight=c(profit=Inf, cost=Inf, turnover=1))
  })

  it ("handles equality in condition",{
    rules <- validator(if (a == 0) b == 0)
    data <- data.frame(a = 0, b = 1)
    le <- locate_errors(data, rules, weight=c(a=2, b=1))
    expect_equivalent(le$errors[1,], c(a = FALSE, b = TRUE))
  })
})
