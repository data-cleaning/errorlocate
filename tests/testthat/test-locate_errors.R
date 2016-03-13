context("locate_errors")

describe("locate_errors", {
  it("works with validator",{
    v <- validator(x > 1)
    dat <- data.frame(x = c(2, 0))
    loc <- fh_localizer(v)
    inherits("ErrorLocalizer", class(loc))
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
})
