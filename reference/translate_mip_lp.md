# Translate linear rules into an LP problem

Translate linear rules into an LP problem

## Usage

``` r
translate_mip_lp(rules, objective = NULL, eps = 0.001, ...)
```

## Arguments

- rules:

  MIP rules

- objective:

  function

- eps:

  accuracy for equality/inequality

- ...:

  additional
  [`lpSolveAPI::lp.control()`](https://rdrr.io/pkg/lpSolveAPI/man/lp.control.html)
  parameters that are set for the MIP problem
