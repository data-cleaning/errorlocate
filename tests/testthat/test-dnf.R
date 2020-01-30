context("dnf")

describe("as_dnf", {
  it("works with simple expressions", {
    dnf <- as_dnf(quote(x > 1))
    expect_equal(dnf[[1]], quote(x > 1))
    expect_equal(length(dnf), 1)

    dnf <- as_dnf(quote(x == 1))
    expect_equal(dnf[[1]], quote(x == 1))
    expect_equal(length(dnf), 1)

    dnf <- as_dnf(quote(A == "a"))
    expect_equal(dnf[[1]], quote(A == "a"))
    expect_equal(length(dnf), 1)
  })

  it("works with if statements", {
    dnf <- as_dnf(quote(if (x > 1) y < 0))
    expect_equivalent(dnf, expression(x <=1, y < 0))


    dnf <- as_dnf(quote(if (A == "a") y < 0))
    expect_equivalent(dnf, expression(A != "a", y < 0))

    dnf <- as_dnf(quote(if (A != "a") y < 0))
    expect_equivalent(dnf, expression(A == "a", y < 0))

    dnf <- as_dnf(quote(if (A %in% "a") y < 0))
    expect_equivalent(dnf, expression(!(A %in% "a"), y < 0))
  })

  it("works with complex if statements", {
    dnf <- as_dnf(quote(if (x > 1 & z > 1) y < 0))
    expect_equal(dnf[[1]], quote(x <= 1))
    expect_equal(dnf[[2]], quote(z <= 1))
    expect_equal(dnf[[3]], quote(y < 0))

    expect_equal(length(dnf), 3)

    dnf <- as_dnf(quote(if (x > 1 & z > 1 & w > 1) y < 0))
    expect_equivalent(dnf, expression(x <=1, z <= 1, w <= 1, y < 0))
  })

})
