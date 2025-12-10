# Check if rules are conditional rules

Check if rules are conditional rules

## Usage

``` r
is_conditional(rules, ...)
```

## Arguments

- rules:

  validator object containing validation rules

- ...:

  not used

## Value

logical indicating which rules are conditional

## Note

`errorlocate` supports linear, categorical and conditional rules to be
used in finding errors. Other rule types are ignored during error
finding.

## See also

Other rule type: [`is_categorical()`](is_categorical.md),
[`is_linear()`](is_linear.md)

## Examples

``` r
v <- validator( A %in% c("a1", "a2")
              , B %in% c("b1", "b2")
              , if (A == "a1")  x > 1 # conditional
              , if (y > 0) x >= 0 # conditional
              , if (A == "a1") B == "b1" # categorical
              )

is_conditional(v)
#> [1] FALSE FALSE  TRUE  TRUE FALSE
```
