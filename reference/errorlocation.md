# Error location object

`errorlocation` contains the result of error localization. It stores,
per cell, whether a value is flagged as erroneous (`TRUE`), valid
(`FALSE`), or missing (`NA`).

- A record-based error is restricted to one observation.
  [`errorlocate()`](errorlocate-package.md) using the Fellegi-Holt
  algorithm assumes errors are record-based.

- A variable-based error is a flaw in a uni- or multivariate
  distribution. Correcting it typically requires changing multiple
  observations or aggregate totals.

## Details

Current implementation assumes record-based errors. Retrieve error
locations with
[`validate::values()`](https://rdrr.io/pkg/validate/man/values.html),
which returns a matrix with the same dimensions as the checked
`data.frame`. For errors that are purely column-based, or dataset-based,
`errorlocation` returns a matrix with all rows or cells set to `TRUE`.
[`validate::values()`](https://rdrr.io/pkg/validate/man/values.html)
returns `NA` for missing values.

## Fields

- `$errors`: `matrix` indicating which values are erroneous (`TRUE`),
  missing (`NA`) or valid (`FALSE`)

- `$weight`: The total weight per record. A weight of 0 means no errors
  were detected.

- `$status`: The
  [status](https://rdrr.io/pkg/lpSolveAPI/man/solve.lpExtPtr.html) of
  the MIP solver for this record.

- `$duration`: The number of seconds for processing each record.

## See also

Other error finding: [`errors_removed()`](errors_removed.md),
[`expand_weights()`](expand_weights.md),
[`locate_errors()`](locate_errors.md),
[`replace_errors()`](replace_errors.md)
