describe("expand_weights",{

  dat <- data.frame(a = c(20,20), b  = c(10,10))
  W_one <- matrix(c(1,1, 1, 1), ncol=2, dimnames = list(NULL, c("a","b")))
  W_ref <- matrix(c(2,2, 1, 1), ncol=2, dimnames = list(NULL, c("a","b")))

  it("accepts a named numeric",{
    weight <- c(a=2, b=1)
    W <- expand_weights(dat, weight)
    expect_equal(W, W_ref)
  })

  it("warns about a named numeric",{
    weight <- c(a=2, c=1)
    expect_warning({
      W <- expand_weights(dat, weight)
    })
    expect_equal(W, W_ref)
  })

  it("accepts an unnamed numeric",{
    weight <- c(2, 1)
    W <- expand_weights(dat, weight)
    expect_equal(W, W_ref)
  })

  it("errors an unnamed numeric of wrong length",{
    weight <- c(2)
    expect_error({
      W <- expand_weights(dat, weight)
    })
  })

  it("returns a data.frame",{
    weight <- c(2, 1)
    W <- expand_weights(dat, weight, as.data.frame = TRUE)
    expect_equal(W, as.data.frame(W_ref))
  })


  it("accepts a matrix",{
    weight <- W_ref
    W <- expand_weights(dat, weight)
    expect_equal(W, W_ref)
  })

  it("accepts a unnamed matrix",{
    weight <- W_ref
    colnames(weight) <- NULL
    W <- expand_weights(dat, weight)
    expect_equal(W, W_ref)
  })

  it("warns on a wrongly named matrix",{
    weight <- W_ref
    colnames(weight) <- c("a","c")
    expect_warning({
      W <- expand_weights(dat, weight)
    })
    expect_equal(W, W_ref)
  })

  it("errors on a matrix with the wrong dimensions",{
    weight <- matrix(1:10, ncol=2)
    expect_error(W <- expand_weights(dat, weight))
  })

  it("errors on a matrix with the wrong dimensions",{
    weight <- matrix(1:10, nrow=2)
    expect_error({
      W <- expand_weights(dat, weight)
    })
  })

  it("accepts a named numeric in different order",{
    weight <- c(b=1, a=2)
    W <- expand_weights(dat, weight)
    expect_equal(W, W_ref)
  })

  it("accepts an incomplete numeric",{
    weight <- c(a=2)
    W <- expand_weights(dat, weight)
    expect_equal(W, W_ref)
  })

  it("accepts a data.frame",{
    weight <- data.frame(a = c(2,2), b = c(1,1))
    W <- expand_weights(dat, weight)
    expect_equal(W, W_ref)
  })

  it("accepts an incomplete data.frame",{
    weight <- data.frame(a = c(2,2))
    W <- expand_weights(dat, weight)
    expect_equal(W, W_ref)
  })

  it("accepts NULL",{
    weight <- NULL
    W <- expand_weights(dat, weight)
    expect_equal(W, W_one)
  })

  it("uses row weights",{
    rules <- validator(a < b)
    weight <- data.frame(a = c(2,1), b = c(1,2))
    el <- locate_errors(dat, rules, weight =weight, Ncpus = 1)
    expect_equal(el$errors
                , matrix(c(FALSE, TRUE, TRUE, FALSE)
                        , ncol=2
                        , dimnames = list(NULL, c("a","b"))
                        )
                )
  })

})
