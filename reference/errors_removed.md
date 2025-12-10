# Get location of removed errors from a 'cleaned' data set

`errors_removed` retrieves the errors detected by
[`replace_errors()`](replace_errors.md)

## Usage

``` r
errors_removed(x, ...)
```

## Arguments

- x:

  `data.frame` that was checked for errors

- ...:

  not used

## Value

[`errorlocation-class()`](errorlocation.md) object

## See also

Other error finding: [`errorlocation-class`](errorlocation.md),
[`expand_weights()`](expand_weights.md),
[`locate_errors()`](locate_errors.md),
[`replace_errors()`](replace_errors.md)

## Examples

``` r
rules <- validator( profit + cost == turnover
              , cost - 0.6*turnover >= 0
              , cost>= 0
              , turnover >= 0
)
data <- data.frame(profit=755, cost=125, turnover=200)

data_no_error <- replace_errors(data,rules)

# faulty data was replaced with NA
data_no_error
#>   profit cost turnover
#> 1     NA  125      200

errors_removed(data_no_error)
#> call:  locate_errors(data, x, ref, ..., cl = cl, Ncpus = Ncpus) 
#> located  1  error(s).
#> located  0  missing value(s).
#> Use 'summary', 'values', '$errors' or '$weight', to explore and retrieve the errors.

# a bit more control, you can supply the result of locate_errors
# to replace_errors, which is a good thing, otherwise replace_errors will call
# locate_errors internally.
error_locations <- locate_errors(data, rules)
replace_errors(data, error_locations)
#>   profit cost turnover
#> 1     NA  125      200
```
