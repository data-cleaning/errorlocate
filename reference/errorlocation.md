# Error location object

Errorlocation contains the result of a error detection. Errors can
record based or variable based.

- A record based error is restricted within one observation.
  [`errorlocate()`](errorlocate-package.md) using the Fellegi Holt
  algorithm assumes errors are record based.

- A variable based error is a flaw in uni- or multivariate distribution.
  To correct this error multiple observations or the aggregated number
  should be adjusted.

## Details

Current implementation assumes that errors are record based. The error
locations can be retrieved using the method
[`validate::values()`](https://rdrr.io/pkg/validate/man/values.html) and
are a matrix of rows and columns, with the same dimensions are the
`data.frame` that was checked. For errors that are purely column based,
or dataset based, errorlocations will return a matrix with all rows or
cells set to `TRUE`. The
[`validate::values()`](https://rdrr.io/pkg/validate/man/values.html)
return `NA` for missing values.

## Fields

- `$errors`: `matrix` indicating which values are erronuous (`TRUE`),
  missing (`NA`) or valid (`FALSE`)

- `$weight`: The total weight per record. A weight of 0 means no errors
  were detected.

- `$status`: The
  [status](https://rdrr.io/pkg/lpSolveAPI/man/solve.lpExtPtr.html) of
  the mip solver for this record.

- `$duration`: The number of seconds for processing each record.

## See also

Other error finding: [`errors_removed()`](errors_removed.md),
[`expand_weights()`](expand_weights.md),
[`locate_errors()`](locate_errors.md),
[`replace_errors()`](replace_errors.md)
