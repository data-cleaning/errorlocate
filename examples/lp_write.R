# existing implementation

library(errorlocate)
rules <- validator(
  a %in% c("A1", "A2"),
  if (b == TRUE) a == "A1"
)

data <- data.frame(a = "A2", b= TRUE)

mip <- inspect_mip(data, rules)
mip$write_lp(filename = "./examples/test.lp")
