# https://github.com/data-cleaning/errorlocate/issues/8

library(validate)

data("retailers")
v <- validator(turnover + other.rev==total.rev)
# el <- locate_errors(retailers[3,4:6],v)
#
# head(el$._values)
# turnover other.rev total.rev
# [1,]    FALSE     FALSE      TRUE
# >
#   > # 1st row has missings
el <- locate_errors(retailers[1,4:6],v)
el
# head(el$._values)
