
#' @export
localize_errors <- function(data){
}


# test 1, 2, 3
dat <- data.frame(x=1:2, y=3:2, a=c("C", "A"))

v <- validator(
  x > 1,
  y > x,
  a %in% c("A", "B")
)

confront(dat, v)

