context("localize-errors")

## TODO: Add more tests
describe("Solve editrules checks",{
  it("solves first editrules mip check",{
    v <- validator( p + c == t
                  , c - 0.6*t >= 0
                  , c >= 0
                  , p >= 0
                  )
    data <- data.frame(p=755, c=125, t=200)
    errors <- locate_errors(data, v)
    expect_equivalent(values(errors), matrix(c(p=TRUE, c=FALSE, t=FALSE), nrow=1))
    expect_equal(errors$weight, 1)
  })
})
