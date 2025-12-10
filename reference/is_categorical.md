# Check if rules are categorical

Check if rules are categorical

## Usage

``` r
is_categorical(x, ...)
```

## Arguments

- x:

  validator or expression object

- ...:

  not used

## Value

logical indicating which rules are purely categorical/logical

## Details

\#' @note `errorlocate` supports linear, categorical and conditional
rules to be used in finding errors. Other rule types are ignored during
error finding.

## See also

Other rule type: [`is_conditional()`](is_conditional.md),
[`is_linear()`](is_linear.md)

## Examples

``` r
v <- validator( A %in% c("a1", "a2")
              , B %in% c("b1", "b2")
              , if (A == "a1") B == "b1"
              , y > x
              )

is_categorical(v)
#> [1]  TRUE  TRUE  TRUE FALSE
```
