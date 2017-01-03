context("errorlocation")

describe("errorlocation",{
  it("can print",{
    rules <- validator(x > 1)
    data <- data.frame(x = 1:2, y=c(2,NA))
    el <- locate_errors(data, rules)

    expect_output_file(print(el), "test-errorlocation-print.txt", update=FALSE)
    expect_equal(el$errors, values(el))
  })
  it("can do a summary",{
    rules <- validator(x > 1)
    data <- data.frame(x = 1:2, y=c(2,NA))
    el <- locate_errors(data, rules)

    el_s <- summary(el)
    expect_output_file(print(el_s), "test-errorlocation-summary.txt", update=FALSE)
    expect_equivalent(as.integer(el_s$record_errors), c(1,1))
    expect_equivalent(as.integer(el_s$record_missing), c(1,1))
  })
  it("has errors_removed", {
    rules <- validator(x > 1)
    data <- data.frame(x = 1:2, y=c(2,NA))
    data_clean <- replace_errors(data, rules)
    el <- errors_removed(data_clean)
    el_s <- summary(el)
    expect_output_file(print(el_s), "test-errorlocation-summary.txt", update=FALSE)
  })
})
