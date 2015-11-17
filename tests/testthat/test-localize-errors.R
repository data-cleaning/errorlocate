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
#     Et <- editmatrix(expression( p + c == t
#                                  , c - 0.6*t >= 0
#                                  , c>=0
#                                  , p >=0
#     )
#     )
#
#     x <- c(p=755,c=125,t=200)
#
#     sol <- errorLocalizer_mip(Et, x)
#     expect_equal(sol$w, 1)
#     expect_equivalent(sol$adapt, c(TRUE, FALSE, FALSE))
#     expect_equal(unname(sol$x_feasible[1:3]), c(75, 125, 200), tolerance=1e-10)
  })
})
