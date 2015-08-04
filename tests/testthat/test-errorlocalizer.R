context("errorlocalizer")

describe("fh_localizer", {
  it("can solve a trivial system",{
    v <- validator(x>1)
    data <- data.frame(x = 2)
    loc <- fh_localizer(v)
    loc$locate(data)
  })
  it("can solve a trivial system 2",{
    v <- validator(x>1)
    data <- data.frame(x = 0)
    loc <- fh_localizer(v)
    loc$locate(data)
  })
  it("can solve a simple system",{
    v <- validator(x > 1, y > x)
    data <- data.frame(x = 2, y = 0)
    loc <- fh_localizer(v)
    el <- loc$locate(data)
    expect_equivalent(values(el)[1,], c(x=FALSE, y=TRUE))
  })
})


describe("locate_errors", {
  it("can solve a trivial system")
})
