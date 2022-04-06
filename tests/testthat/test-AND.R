describe("AND-ing rules work",{
  it("can work with simple AND", {
    rules <- validator(a == TRUE & b == TRUE)
    d <- data.frame(a = TRUE, b = FALSE)
    le <- locate_errors(d, rules)
    expect_equivalent(values(le)[1,], c(FALSE, TRUE))
  })

  it("can work with simple AND", {
    rules <- validator(x > 1 & b == TRUE)
    d <- data.frame(x= 3, b = FALSE)
    le <- locate_errors(d, rules)
    expect_equal(values(le)[1,], c(x=FALSE, b=TRUE))
  })

  it("can work with nested AND in consequent", {
    rules <- validator(if (b == TRUE) x > 1 & y > 1)
    d <- data.frame(b = TRUE, x =3 , y = 0)
    le <- locate_errors(d, rules, weight=c(b=Inf))
    expect_equal(values(le)[1,], c(b=FALSE, x=FALSE, y=TRUE))
  })

  it("can work with nested OR in the if part", {
    skip("nested OR not working")
    rules <- validator(if (x1 < 1 && x2 < 1) x3 >= 1 & x4 >= 1)
    d <- data.frame(x1 = 0, x2 = 0, x3 = 0, x4 = 0)
    le <- locate_errors(d, rules, weight=c(x1=Inf))
    expect_equal(values(le)[1,], c(x1=FALSE, x2-FALSE, x3=TRUE, x4=TRUE))
  })



})
