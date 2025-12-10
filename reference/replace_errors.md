# Replace erroneous fields with NA or a suggested value

Find erroneous fields using [`locate_errors()`](locate_errors.md) and
replace these fields automatically with NA or a suggestion that is
provided by the error detection algorithm.

## Usage

``` r
replace_errors(
  data,
  x,
  ref = NULL,
  ...,
  cl = NULL,
  Ncpus = getOption("Ncpus", 1),
  value = c("NA", "suggestion")
)

# S4 method for class 'data.frame,validator'
replace_errors(
  data,
  x,
  ref = NULL,
  ...,
  cl = NULL,
  Ncpus = getOption("Ncpus", 1),
  value = c("NA", "suggestion")
)

# S4 method for class 'data.frame,ErrorLocalizer'
replace_errors(
  data,
  x,
  ref = NULL,
  ...,
  cl = NULL,
  Ncpus = getOption("Ncpus", 1),
  value = c("NA", "suggestion")
)

# S4 method for class 'data.frame,errorlocation'
replace_errors(
  data,
  x,
  ref = NULL,
  ...,
  cl = NULL,
  Ncpus = 1,
  value = c("NA", "suggestion")
)
```

## Arguments

- data:

  data to be checked

- x:

  [`validate::validator()`](https://rdrr.io/pkg/validate/man/validator.html)
  or `errorlocation` object. If an `errorlocation` is already available
  (through [`locate_errors()`](locate_errors.md)) this is more
  efficient.

- ref:

  optional reference data set

- ...:

  these parameters are handed over to
  [`locate_errors()`](locate_errors.md)

- cl:

  optional cluster for parallel execution (see details)

- Ncpus:

  number of nodes to use. (see details)

- value:

  `NA`

## Value

`data` with erroneous values removed.

## Details

Note that you can also use the result of
[`locate_errors()`](locate_errors.md) with `replace_errors`. When the
procedure takes a long time and `locate_errors` was called previously
this is the preferred way, because otherwise `locate_errors` will be
executed again. The errors that were removed from the `data.frame` can
be retrieved with the function [`errors_removed()`](errors_removed.md).
For more control over error localization see
[`locate_errors()`](locate_errors.md).

`replace_errors` has the same parallelization options as
[`locate_errors()`](locate_errors.md) (see there).

## Note

In general it is better to replace the erroneous fields with `NA` and
apply a proper imputation method. Suggested values from the error
localization method may introduce an undesired bias.

## See also

[`errorlocation-class()`](errorlocation.md)

Other error finding: [`errorlocation-class`](errorlocation.md),
[`errors_removed()`](errors_removed.md),
[`expand_weights()`](expand_weights.md),
[`locate_errors()`](locate_errors.md)

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
