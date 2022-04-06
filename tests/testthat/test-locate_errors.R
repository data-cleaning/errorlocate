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
    v <- validator( profit + cost == turnover
                  , cost/turnover >= 0.6
                  , cost>= 0
                  , turnover >= 0
    )
    data <- data.frame(profit=30, cost=70, turnover=80)
    set.seed(1)
    le <- locate_errors(data, v, weight=c(profit=Inf, cost=1, turnover=Inf))
    expect_equivalent(le$errors[1,], c(profit = FALSE, cost = TRUE, turnover = FALSE))

    le <- locate_errors(data, v, weight=c(profit=Inf, cost=Inf, turnover=1))
    expect_equivalent(le$errors[1,], c(profit = FALSE, cost = FALSE, turnover = TRUE))
  })

  it ("handles equality in condition",{
    rules <- validator(if (a == 0) b == 0)
    data <- data.frame(a = 0, b = 1)
    le <- locate_errors(data, rules, weight=c(a=2, b=1))
    expect_equivalent(le$errors[1,], c(a = FALSE, b = TRUE))
  })

  it ("handles a strict equality", {
    rules <- validator(x == 0)
    data <- data.frame(x = 0)
    le <- locate_errors(data, rules)
    expect_equivalent(le$errors[1,], FALSE)

    data <- data.frame(x = 1)
    le <- locate_errors(data, rules)
    expect_equivalent(le$errors[1,], TRUE)
  })

  it ("errors on missing columns",{
    rules <- validator(x > 0, y > 0)
    data <- data.frame(y = 1)
    expect_error(
      le <- locate_errors(data, rules),
      "Missing column"
    )
  })

  it ("can handle a wrong category", {
    rules <- validator( x %in% c("B","A"))
    data <- data.frame(x = "C")
    res <- locate_errors(data, rules)$errors
    expect_true(res)
  })

  it ("can handle a single category (issue #25", {
    rules <- validator( x %in% c("A"))
    data <- data.frame(x = "C")
    res <- locate_errors(data, rules)$errors
    expect_true(res)
  })

  it ("errors on using integer variables as categories",{
    rules <- validator(if (sector %in% c(1,2)) turnover > 0)

    # faulty record
    data <- data.frame(sector = 1, turnover = 0)
    weight <- c(sector = 2, turnover = 1)

    # no errors found, but a warning is given
    expect_warning({
      expect_error({
        el <- locate_errors(data, rules, weight=weight)$errors
      })
    })

    # recoding as factor
    data$sector <- factor(data$sector)
    expect_warning({
        el <- locate_errors(data, rules, weight=weight)$errors
    })


  })

  it ("handles NA logical values",{
    rules <- validator(if (age >=18) adult == TRUE)
    data <- data.frame(age = 15, adult=NA)
    el <- locate_errors(data, rules)$errors
    expect_equal(el[1,], c(age=FALSE, adult=NA))
  })

  it ("rewrites ratio",{
    rules <- validator( x / y <= 1, x > 5)
    data <- data.frame(x = 4L, y = 10L)
    el <- locate_errors(data, rules)
    expect_equal(as.logical(el$errors), c(TRUE, FALSE))
  })

  it ("handles variables that are not part of the linear rules gracefully",{
    rules <- validator( sin(x) <= 1, x > 5)
    data <- data.frame(x = 1L, y = 2L)
    expect_warning({
      el <- locate_errors(data, rules)
    })
    expect_equal(as.logical(el$errors), c(TRUE, FALSE))
  })

  it ("handles a contradiction in log constraints ",{
    options(errorlocate.allow_log = TRUE)

    rules <- validator(log(x) > log(4), x < 3)
    d <- data.frame(x = 2)
    expect_warning (
      el <- locate_errors(d, rules)
    )

    options(errorlocate.allow_log = NULL)

  })

  it ("handles small log transformed variables ",{
    options(errorlocate.allow_log = TRUE)

    rules <- validator(log(x) > log(.1), x < 3)
    d <- data.frame(x = 0.3)
    el <- locate_errors(d, rules)

    expect_false(as.logical(el$errors))

    rules <- validator(log(x) > log(1), x < 3)
    d <- data.frame(x = 1)
    el <- locate_errors(d, rules)
    mip <- inspect_mip(d, rules)
    s <- mip$execute()

    expect_true(as.logical(el$errors))

    options(errorlocate.allow_log = NULL)

  })

  it("handles values >=1e7 gracefully, issue #30", {
    rules <- validator( profit == turnover - cost
                        , cost >= 0.6 * turnover
                        , turnover >= 0
    )

    data <- data.frame(profit = 1e10, cost = 200, turnover = 300)

    expect_warning({
      el <- locate_errors(data, rules)
    })
    expect_equal(el$errors[1,], c(profit=NA, cost=FALSE, turnover=FALSE))

    expect_warning({
      data_na <- replace_errors(data, rules)
    })
    expect_equivalent(data_na[1,], data.frame(profit=NA_real_, cost=200, turnover=300))
  })

  it ("handles NA values gracefully, issue #31", {
    d <- data.frame(x = 10, y = NA, z = 5)
    rules <- validator( x + y == z, x >= 0, y >= 0)

    set.seed(1)
    el <- locate_errors(d, rules)
    expect_equal(el$errors[1,], c(x=TRUE, y = NA, z = FALSE))
  })

  it ("handles inf weights", {
    d <- data.frame(x = 6, y = 6, z = 5)
    rules <- validator( x + y == z, x >= 0, y >= 0)

    set.seed(1)
    weight <- c(x = 1, y = 1, z = Inf)
    el <- locate_errors(d, rules, weight = weight)
    expect_equal(el$errors[1,], c(x=TRUE, y = TRUE, z = FALSE))

  })

  it ("handles inf weights", {
    d <- data.frame(x = 6, y = 6, z = 5)
    rules <- validator( x + y == z, x >= 0, y >= 0)

    set.seed(1)
    weight <- c(x = 1, y = 1, z = Inf)
    el <- locate_errors(d, rules, weight = weight)
    expect_equal(el$errors[1,], c(x=TRUE, y = TRUE, z = FALSE))

  })

  it ("handles issue 34 (non aggressively)",{
    d <- data.frame(y = 60, yA = 5, yB = 5, yC = 5, yD = 45, yE = 0)

    rules <- validator(60 == y,
                        5 == yA,
                        5 == yB,
                        5 == yC,
                        25 == yD,
                        20 == yE,
                        y == yA + yB + yC + yD + yE)
    set.seed(1)
    le <- locate_errors(d, rules)
    expect_equal( le$errors[1,]
                  , c(y=FALSE, yA=FALSE, yB=FALSE, yC=FALSE, yD=TRUE, yE=TRUE)
    )
  })

  it("accepts a in_range function", {
    rules <- validator(in_range(age, 18, 67))
    d <- data.frame(age = c(0, 100, 50))
    le <- locate_errors(d, rules)
    expect_equal(le$errors[, 1], c(TRUE, TRUE, FALSE))
  })

  it("deals with large values and NA (issue #33)", {
    data <- iris[1,]
    data$Petal.Width <- NA_real_
    data$Petal.Length <- 1e7 + 1

    rules <- validator(Petal.Width > 0, Petal.Length > 0)
    expect_warning({
      le <- locate_errors(data, rules)
    })
    expect_equal(le$errors[1,], c( Sepal.Length = FALSE
                                 , Sepal.Width = FALSE
                                 , Petal.Length = NA
                                 , Petal.Width = NA
                                 , Species = FALSE
                                 )
    )
  })
})
