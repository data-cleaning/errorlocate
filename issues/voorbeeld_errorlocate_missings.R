
library(validate)
library(validatetools)
library(errorlocate)

VV <-  validator(
  # ytot == 60,
  # ya == 5,
  # yb == 5,
  # yc == 5,
  # yd == 25,
  # ye == 20,
  ytot == ya + yb + yc + yd + ye,
  ya >= 0,
  yb >= 0,
  yc >= 0,
  yd >= 0,
  ye >= 0
)
VV

xx <- data.frame(ytot = 60,
                 ya = 5,
                 yb = 5,
                 yc = 5,
                 yd = NA_real_,
                 ye = 0)
xx

ll <- locate_errors(data = xx, x = VV)
summary(ll)
ll$weight
values(ll)

## Resultaat:

# > summary(ll)
# Variable:
#   name errors missing
# 6   ye      1       0
# 1 ytot      0       1
# 2   ya      0       1
# 3   yb      0       1
# 4   yc      0       1
# 5   yd      0       1
# Errors per record:
#   errors records
# 1      1       1

# > ll$weight
# [1] NA

# > values(ll)
#      ytot ya yb yc yd   ye
# [1,]   NA NA NA NA NA TRUE
