context("miprule")

describe("get_mr_type",{
  it("can return correct type",{
    v <- validator(x > 1, A %in% 'b', x + y==3)
    rules <- c(cat_as_mip_rules(v), lin_as_mip_rules(v))
    expect_equal(get_mr_type(rules), c("A:b"="binary", x="double", y="double"))
  })
  it("can print", {
    v <- validator(x > 1, A %in% 'b', x + y==3)
    rules <- c(cat_as_mip_rules(v), lin_as_mip_rules(v))
    expect_output(print(rules))
  })
  it("can be an expression",{
    rules <- validator( x > 1
                      , A %in% 'b'
                      , x + y==3
                      )
    #mr <- miprules(rules)$mip_rules()
    #get_mr_expression(mr)
    #get_mr_weights(mr)
  })
})

describe("print mr rule", {
  rules <- validator( x  < 1.1*y, 2*x  < y)
  mr <- to_miprules(rules)
  expect_output(print(mr[[1]]), "V1: x - 1.1*y < 0", fixed = TRUE)
  expect_output(print(mr[[2]]), "V2: 2*x - y < 0", fixed = TRUE)
})

describe("print mr rule", {
  rules <- validator( in_range(age, 18, 67))
  mr <- to_miprules(rules)

  expect_output(print(mr[[1]]), "V1: -age <= -18", fixed = TRUE)
  expect_output(print(mr[[2]]), "V1: age <= 67", fixed = TRUE)
})

describe("l_constraint",{
  it("can create an l constraint", {
    rules <- validator( x < 1, y == 2, x < y
                      )
    mr <- to_miprules(rules)
    l <- get_mr_l_constraint(mr)
    expect_equal(l$dir, c("<", "==", "<"))
    expect_equal(l$rhs, c(1,2,0))
    expect_known_value(as.matrix(l$L), file = "l_constraint_matrix.rds")
  })

  it("can create an op", {
    rules <- validator( x <= 1, y == 2, x <= y
                      , if (A == "a1") B == "b1"
                      , A %in% c("a1", "a2")
                      , B %in% c("b1", "b2")
    )
    mr <- to_miprules(rules)
    op <- translate_mip_OP(rules = mr)

    expect_equal(as.matrix(op$objective$L), matrix(0, nrow=1, ncol=2))
    expect_equal(op$objective$names, c("x", "y"))

    op$constraints$L |> as.matrix()
    op$constraints$L$v

    ROI::ROI_require_solver("lpsolve")
    sol <- ROI::ROI_solve(op, solver = "lpsolve")
    sol$status

    ROI::ROI_require_solver("highs")
    sol <- ROI::ROI_solve(op, solver = "highs")
    sol$status

    # sol <- ROI::ROI_solve(op, solver = "ecos")
    # sol$status

  })
})

