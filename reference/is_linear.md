# Check which rules are linear rules.

Check which rules are linear rules.

## Usage

``` r
is_linear(x, ...)
```

## Arguments

- x:

  [`validate::validator()`](https://rdrr.io/pkg/validate/man/validator.html)
  object containing data validation rules

- ...:

  not used

## Value

`logical` indicating which rules are (purely) linear.

## Note

`errorlocate` supports linear, categorical and conditional rules to be
used in finding errors. Other rule types are ignored during error
finding.

## See also

Other rule type: [`is_categorical()`](is_categorical.md),
[`is_conditional()`](is_conditional.md)

## Examples

``` r
v <- validator( A %in% c("a1", "a2")
              , B %in% c("b1", "b2")
              , if (A == "a1") B == "b1"
              , y > x
              , z + 1 < 2*x + 3*y
              )

is_linear(v)
#> [1] FALSE FALSE FALSE  TRUE  TRUE
```
