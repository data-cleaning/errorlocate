soft_edits <- function(){
}

v <- validator( x> 1, x < 10, y >=3, y <= 40, if (A) B %in% c('b1','b2'))
v$linear_coefficients()
cat_coefficients(v)
