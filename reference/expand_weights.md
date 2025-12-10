# Create a weight matrix

Expands a weight specification into a weight matrix to be used by
`locate_errors` and `replace_errors`. Weights allow for "guiding" the
errorlocalization process, so that less reliable values/variables with
less weight are selected first. See details on the specification.

## Usage

``` r
expand_weights(dat, weight = NULL, as.data.frame = FALSE, ...)
```

## Arguments

- dat:

  `data.frame` the data to be checked

- weight:

  weight specification, see details.

- as.data.frame:

  if `TRUE` a `data.frame` will be returned.

- ...:

  unused

## Value

`matrix` or `data.frame` of same dimensions as `dat`

## Details

If weight fine tuning is needed, a possible scenario is to generate a
weight `data.frame` using `expand_weights` and adjust it before
executing [`locate_errors()`](locate_errors.md) or
[`replace_errors()`](replace_errors.md). The following specifications
for `weight` are supported:

- `NULL`: generates a weight matrix with `1`'s

- a named `numeric`, unmentioned columns will have weight 1

- a unnamed `numeric` with a length equal to `ncol(dat)`

- a `data.frame` with same number of rows as `dat`

- a `matrix` with same number of rows as `dat`

- `Inf`, `NA` weights will be interpreted as that those variables must
  not be changed and are fixated. `Inf` weights perform much better than
  setting a weight to a large number.

## See also

Other error finding: [`errorlocation-class`](errorlocation.md),
[`errors_removed()`](errors_removed.md),
[`locate_errors()`](locate_errors.md),
[`replace_errors()`](replace_errors.md)

## Examples

``` r
dat <- read.csv(text=
"age,country
  49,     NL
  23,     DE
", strip.white=TRUE)

weight <- c(age = 2, country = 1)
expand_weights(dat, weight)
#>      age country
#> [1,]   2       1
#> [2,]   2       1

weight <- c(2, 1)
expand_weights(dat, weight, as.data.frame = TRUE)
#>   age country
#> 1   2       1
#> 2   2       1

# works too
weight <- c(country=5)
expand_weights(dat, weight)
#>      age country
#> [1,]   1       5
#> [2,]   1       5

# specify a per row weight for country
weight <- data.frame(country=c(1,5))
expand_weights(dat, weight)
#>      age country
#> [1,]   1       1
#> [2,]   1       5

# country should not be changed!
weight <- c(country = Inf)
expand_weights(dat, weight)
#>      age country
#> [1,]   1     Inf
#> [2,]   1     Inf
```
