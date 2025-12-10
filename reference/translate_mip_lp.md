# translate linear rules into an lp problem

translate linear rules into an lp problem

## Usage

``` r
translate_mip_lp(rules, objective = NULL, eps = 0.001, ...)
```

## Arguments

- rules:

  mip rules

- objective:

  function

- eps:

  accuracy for equality/inequality

- ...:

  additional
  [`lpSolveAPI::lp.control()`](https://rdrr.io/pkg/lpSolveAPI/man/lp.control.html)
  parameters that are set for the mip problem
