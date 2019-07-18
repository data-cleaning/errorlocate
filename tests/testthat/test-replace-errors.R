context("replace_errors")

describe("replace errors",{
  it("works with linear rules",{
    rules <- validator( profit + cost == turnover
                    , cost - 0.6*turnover >= 0
                    , cost>= 0
                    , profit >= 0
    )
    data <- data.frame(profit=755, cost=125, turnover=200)

    data_no_errors <- replace_errors(data, rules)
    expect_equivalent(data_no_errors, c(profit=NA_real_, cost=data$cost, turnover=data$turnover))
  })

  it("works with weights",{
    rules <- validator( profit + cost == turnover
                        , cost - 0.6*turnover >= 0
                        , cost>= 0
                        , profit >= 0
    )
    data <- data.frame(profit=55, cost=125, turnover=200)
    weight <- c(profit=2,cost=2,turnover=1)
    data_no_errors <- replace_errors(data, rules, weight=weight)
    expect_equivalent(data_no_errors, c(profit=data$profit, cost=data$cost, turnover=NA_real_))

    weight <- c(profit=2,cost=1,turnover=2)
    data_no_errors <- replace_errors(data, rules, weight=weight)
    expect_equivalent(data_no_errors, c(profit=data$profit, cost=NA_real_, turnover=data$turnover))

    weight <- c(profit=1,cost=2,turnover=2)
    data_no_errors <- replace_errors(data, rules, weight=weight)
    expect_equivalent(data_no_errors, c(profit=NA_real_, cost=data$cost, turnover=data$turnover))
  })

  it("works with NA values",{
    rules <- validator( profit + cost == turnover
                        , cost - 0.6*turnover >= 0
                        , cost>= 0
                        , profit >= 0
    )
    data <- data.frame(profit=NA_real_, cost=125, turnover=200)
    data_no_errors <- replace_errors(data, rules)
    expect_equivalent(data_no_errors, data)
  })

  it("works with categorical rules",{
    rules <- validator( A %in% c("a1", "a2")
                                , B %in% c("b1", "b2")
                                ,  if (A == "a1") B == "b1"
    )

    set.seed(42) # because of random noise added to weight
    data <- data.frame(A = c("a1", "a2"), B = c("b2", "b2"), stringsAsFactors = FALSE)

    le <- locate_errors(data, rules)
    data_no_error <-
      replace_errors(data, rules, weight=c(1,2))

    expect_equivalent(data_no_error, data.frame(A = c(NA, "a2"), B=data$B, stringsAsFactors = FALSE))
  })

  it("works with var_group", {
    df <- data.frame(a=-1, b=0)
    rules <- validator(var_group(a,b)>=0)
    data_no_error <- replace_errors(df, rules)
    expect_equivalent(data_no_error, data.frame(A = NA_real_, b = 0))
  })

  it("works with assignment", {
    df <- data.frame(a=-1, b=0)
    rules <- validator(G := var_group(a,b), G >= 0)
    data_no_error <- replace_errors(df, rules)
    expect_equivalent(data_no_error, data.frame(A = NA_real_, b = 0))
  })

})
