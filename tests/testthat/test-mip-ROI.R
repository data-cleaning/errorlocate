context("ROI")

expect_matrix <- function(x, expected, ...){
  require(slam)
  L = as.matrix(x$L)
  expect_equal(L, expected, ...)
}


describe("ROI",{
  it("can translate simple lin rule", {
    v <- validator(3*x <= 2)

    lin_rules <- errorlocate:::lin_as_mip_rules(v)
    exp <- list( a = 3,
                 op = "<=",
                 b = 2,
                 rule = "V1",
                 type = "double",
                 weight = Inf)

    expect_equivalent(lin_rules[[1]], exp)
  })

  it("can translate lin rules to ROI object", {
    v <- validator(x <= 10, y <= 3, y >= 0, x + y > 2)
    lin_rules <- errorlocate:::lin_as_mip_rules(v)
    op <- errorlocate:::translate_mip_OP(lin_rules)
    expect_equal( op$bounds$lower,
                  list ( ind = 1,
                         val = -Inf
                       )
                )

    expect_equal(op$bounds$upper, list( ind = 1:2
                                      , val = c(10,3)
                                      )
                )

    expect_equal( op$constraints$dir,
                  c("<=", "<=", "<=", "<=")
    )

    L_exp <- matrix(c(1,0,0,-1, 0, 1, -1, -1)
                    , ncol=2
                    , dimnames = list(
                      rule = c("V1", "V2", "V3", "V4"),
                      variable = c("x", "y")
                    ))
    expect_matrix(op$constraints, L_exp)
  })

  it("can encode weights in the objective function",{
    v <- validator(x + y == z, 2*y + 1 == z)
    data <- data.frame(x = 1, y = 1, z = 3)
    mip <- miprules(v)
    set.seed(1)
    mip$set_values(as.list(data), weights = c(x=10, y = 1, z = 1))
    op <- mip$to_OP()
    op$objective$L |> as.matrix()
    op$objective$names
    expect_equal( op$types,
                  c( x = "C", y = "C", z = "C",
                    .delta_x = "B", .delta_y = "B", .delta_z = "B"
                   )
                )
    })

  it("has names for types", {
    rules <- validator(x > 1, if (y < 2) x < 3)
    data <- data.frame(x = 5, y = 1)
    mip <- miprules(rules)
    set.seed(1)
    mip$set_values(as.list(data), weights = c(x=10, y = 1))
    op <- mip$to_OP()
  })

  it ("extracts cat_constraints",{

  })
})



