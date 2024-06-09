
# experimental undocumented option
options(errorlocate.allow_log = TRUE)

rules <- validator( area == width * height  # non linear rule, so ignored
                  , log(area) == log(width) + log(height) # linearized version of first rule
                  , area > 0, width > 0, height > 0
                  )
d <- data.frame(height = 2, width = 3, area = 8)

# we have all confidence in height, a bit in width and not so much in area
le <- locate_errors(d, rules, weight=c(height = Inf, width = 2, area = 1))
le$errors # this works!

# see the "ugly" translation: it tries to keep the log transformed variables "in sync"
# with the variables, meaning that when the mip problem changes the value of e.g.
# area, the log area is also changed / constrained (and vice versa)
mip <- inspect_mip(d, rules)
f <- tempfile()
mip$write_lp(f)
file.edit(f)

# now after the solution has been found (i.e. simplified the problem)
s <- mip$execute()
lpSolveAPI::write.lp(s$lp, f)
file.edit(f)
