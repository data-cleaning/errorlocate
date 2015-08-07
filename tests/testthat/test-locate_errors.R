context("locate_errors")

describe("locate_errors", {
  it("works with validator",{
    v <- validator(x > 1)
    dat <- data.frame(x = c(2, 0))
    loc <- fh_localizer(v)
    inherits("ErrorLocalizer", class(loc))
    locate_errors(dat, loc)
  })
})
