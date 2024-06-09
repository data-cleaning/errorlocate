print_OP <- function(op, ...){
  # TODO check if its a linear constraint

  x <- op$constraints
  rn <- with(x$L, {
    ds <- data.frame(i, j, v) |> split(i)
    sapply(ds, function(d){
        .i <- d$i[1]
        n <- x$names[d$j]
        v <- d$v

        sprintf("%s %s %g"
              , paste0(v, "*", n, collapse = " + ")
              , x$dir[.i]
              , x$rhs[.i]
        )
      })
  })
  names(rn) <- rownames(op$constraints$L)
  rn

  bnds <- op$objective
  bnds$L |> as.matrix()
  op$types
}
