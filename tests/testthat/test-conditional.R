context("conditional rules")

describe("is_condition_",{
  it("can detect a complex rule", {
    e <- quote(y == 1 | z <= 1)
    expect_true(is_condition_(e))
  })
  it("can use negated expression", {
    e <- quote(!y == 1 | z <= 1)
    expect_true(is_condition_(e))
  })
  it("can use nested expression", {
    e <- quote((y == 1) | z <= 1)
    expect_true(is_condition_(e))
  })
  it("can use negated nested expression", {
    e <- quote(!(y == 1 & x > 1) | z <= 1)
    expect_true(is_condition_(e))
  })
})

describe("conditional", {
  it("can detect conditional rules", {
    v <- validator( if ( x > 1) y == 1,
                    if (x > 1) a == "a1",
                    a %in% c("a1", "a2"), # pure categorical
                    if (A=="a") B == "b", # pure categorical
                    x > 1                 # pure linear
    )
    expect_equal( is_conditional(v)
                , c(TRUE, TRUE, FALSE, FALSE, FALSE))
  })
  it("can detect var_group conditional rules",{
    v <- validator( var_group(a,b) >= 0
                  , if (var_group(a,b) == "a") c == TRUE
                  , if (var_group(a,b) > 1) c == TRUE
                  )
   expect_equal(is_conditional(v), c(FALSE, FALSE, TRUE))
  })

  it("can detect more complex rules", {
    v <- validator( if ( x > 1 && z >= 1) y == 1,
                    if (x > 1) y == 1 || z <= 1,
                    if (x > 1) y == 1 | z <= 1,
                    y == 1 | z <= 1
    )
    expect_equal( is_conditional(v)
                  , c(TRUE, TRUE, TRUE, TRUE))
  })
})

describe("replace_linear", {
  it("can replace linear expressions",{
    e <- quote(if (x>1) A==TRUE)
    rl <- replace_linear(e)
    expect_equal(deparse(rl$cat), "if (.v1) A == TRUE")
    expect_equal(deparse(rl$linear$.v1), "x <= 1")
  })
  it("can replace linear expressions",{
    e <- quote(if (x>1) !(y > 1))
    rl <- replace_linear(e)
    expect_equal(deparse(rl$cat), "if (.v1) !(.v2)")
    expect_equal(deparse(rl$linear$.v1), "x <= 1")
    expect_equal(deparse(rl$linear$.v2), "y <= 1")
  })
  it ("transforms simple rule", {
    e <- quote(if(x > 1) y > 2)
    rl <- replace_linear(e)
    expect_equal(deparse(rl$cat), "if (.v1) !.v2")
    expect_equal(length(rl$linear), 2)
    expect_equal(deparse(rl$linear$.v1), "x <= 1")
    expect_equal(deparse(rl$linear$.v2), "y > 2")
  })
})

describe("cond_as_mip_rules",{
  it("transforms simple rule",{
    v <- validator(if(x>1) y>2)
    mr <- cond_as_mip_rules(v)
    expect_equal(length(mr), 3)
    expect_equal(mr[[1]]$rule, "V1")
    expect_equal(mr[[2]]$rule, "V1._lin1")
    expect_equal(mr[[3]]$rule, "V1._lin2")
    get_mr_matrix(mr)
  })

  it("transforms a categorical rule",{
    rules <- validator(A %in% c("a1", "a2"), if (A != "a1") x > 1)
    mr <- to_miprules(rules)
    expect_equal(mr[[2]]$a, c("A:a1" = -1, V2._lin1 = 1))
    expect_equal(mr[[2]]$b, 0)
    expect_equal(mr[[3]]$a, c(x = -1, V2._lin1 = -1e7))
    expect_equal(mr[[3]]$b, -1)
  })

  it("transforms a logical rule", {
    rules <- validator(if (a == FALSE) x > 1)
    mr <- to_miprules(rules)
    expect_equal(mr[[1]]$a, c(a = -1, V1._lin1 = 1))
    expect_equal(mr[[1]]$b, 0)
    expect_equal(mr[[2]]$a, c(x = -1, V1._lin1 = -1e7))
    expect_equal(mr[[2]]$b, -1)
  })
})
