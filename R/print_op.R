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
  # some substitution to improve readability
  rn <- gsub("\\+ \\-", "- ", rn)
  rn <- gsub("\\b1\\*", "", rn)
  rn <- gsub("\\-0\\b", "0", rn)
  names(rn) <- rownames(op$constraints$L)
  rn |> writeLines()
}
