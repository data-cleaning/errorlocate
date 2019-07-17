context("localize-errors (from editrules)")

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
    expect_equivalent(errors$weight, 1)
  })

  it('works without specified weight',{
    expect_equivalent(locate_errors(
      data = data.frame(
        x = c(1,1,1),
        y = c(1,1,1),
        z = c(1,1,1)
      ),
      x = validator(x+y==z,x<1)
    )$errors,
    matrix(c(
      TRUE , FALSE, FALSE,
      TRUE , FALSE, FALSE,
      TRUE , FALSE, FALSE),
      nrow=3,
      byrow=TRUE
    )
    )
  })
  it('works with single specified weight',{
    expect_equivalent(locate_errors(
      x     = validator(x+y==z),
      data  = data.frame(
        x = c(1,1,1),
        y = c(1,1,1),
        z = c(1,1,1)
      ),
      weight  = c(1,2,2),
    )$errors,
    matrix(c(
      TRUE , FALSE, FALSE,
      TRUE , FALSE, FALSE,
      TRUE , FALSE, FALSE),
      nrow=3,
      byrow=TRUE
    )
    )
  })

  it('works with different weights per record',{
    error_loc <-
      locate_errors(
        x       = validator(x+y==z),
        data     = data.frame(
          x = c(1,1,1),
          y = c(1,1,1),
          z = c(1,1,1)
        ),
        weight  = matrix(c(
          1,2,2,
          2,1,2,
          2,2,1),
          nrow=3,
          byrow=TRUE
        )
      )

    expect_equivalent(error_loc$errors,
      matrix(c(
        TRUE , FALSE, FALSE,
        FALSE, TRUE , FALSE ,
        FALSE, FALSE, TRUE),
        nrow=3,
        byrow=TRUE
      )
    )

    expect_that(locate_errors(
      x = editmatrix(x +y==z),
      data = data.frame(
        x = c(1,1,1),
        y = c(1,1,1),
        z = c(1,1,1)
      ),
      weight  = matrix(c(
        1,2,2,
        2,2,1),
        nrow=3,
        byrow=TRUE
      )
    ),
    throws_error()
    )
  })
  it('handles data out-of-datamodel correctly',{

    # thanks to Elmar Wein for sending us this testcase.
    rules <- validator(
      age %in% c('under aged','adult'),
      maritalStatus %in% c('unmarried','married','widowed','divorced'),
      positionInHousehold %in% c('marriage partner', 'child', 'other'),
      if( age == 'under aged' ) maritalStatus == 'unmarried',
      if( maritalStatus %in% c('married','widowed','divorced')) !positionInHousehold %in% c('marriage partner','child')
    )
    record <- data.frame(age='under aged', maritalStatus='unmarried', positionInHousehold='out_of_range')
    expect_equivalent(
      locate_errors(record, rules)$errors,
      array(c(FALSE,FALSE,TRUE),dim=c(1,3))
    )
  })

  it("works with TRUE/FALSE",{
    rules <- validator(
      A %in% c(TRUE,FALSE),
      B %in% letters[1:4],
      if ( A == FALSE) B %in% letters[1:2]
    )

    # should run without errors...
    locate_errors(data.frame(A=c(TRUE,FALSE),B=c('c',"d")), rules)
  })

  it("works with mixed edit",{
    rules <- validator(
      married %in% c(TRUE,FALSE),
      if (married==TRUE) age >=17
    )

    # note bb is switched off for mixed edits
    le <- locate_errors(data.frame(married=TRUE, age=9), rules)
    expect_equal(sum(le$errors), 1)
  })
  it("works for simple numerical if-else", {
    rules <- validator(if ( x > 0 ) y > 0)
    data <-  data.frame( x = 1
                       , y = 0
                       )

    le <- locate_errors(
      data = data,
      x    = rules
    )

    expect_equal(sum(le$errors),1)
  })
  it("works for finding 'out of domain' categories",{
    rules <- validator(a %in% c("A","B"))
    data <- data.frame(a = c("A","C"))
    le <- locate_errors(
      data = data,
      x = rules
    )
    expect_equal(sum(le$errors), 1)

  })
  it("works for var_group rules",{
    rules <- validator(var_group(a,b) >= 0)
    data <- data.frame(a = -1, b = 1)
    le <- locate_errors(data, rules)
    expect_equal(sum(le$errors), 1)
  })
})
