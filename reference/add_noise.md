# Add (a small amount of) noise

Utility function to add some small positive noise to weights. This is
mainly done to randomly choose between solutions of equal weight.
Without adding noise to weights, LP solvers may return an identical
solution over and over while there are multiple solutions of equal
weight. The generated noise is positive to prevent weights from becoming
zero or negative.

## Usage

``` r
add_noise(x, max_delta = NULL, ...)
```

## Arguments

- x:

  `numeric` vector or matrix. When `x` is a matrix, the function will be
  applied to each row of the matrix.

- max_delta:

  when supplied noise will be drawn from `[0,max_delta]` otherwise see
  details

- ...:

  currently not used

## Value

`numeric` vector/matrix with noise applied.

## Details

When no `max_delta` is supplied, add_noise will use the minimum
difference larger than zero divided by the `length(x)`.

## Examples

``` r
x <- c(1, 1, 3, 8)

set.seed(123)
add_noise(x)
#> [1] 1.071894 1.197076 3.102244 8.220754

m <- rbind(c(1, 2, 3), c(1, 2, 3))
set.seed(123)
add_noise(m, max_delta = 0.05)
#>          [,1]     [,2]     [,3]
#> [1,] 1.014379 2.039415 3.020449
#> [2,] 1.044151 2.047023 3.002278
```
