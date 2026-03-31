# Create a mip object from a validator object

Create a `MipRules` object from
[`validate::validator()`](https://rdrr.io/pkg/validate/man/validator.html)
rules. This utility class translates rules into a mixed integer problem.

## Details

Most users should use [`locate_errors()`](locate_errors.md), which
handles translation and execution automatically. `MipRules` is mainly
for advanced users who want to inspect or customize the optimization
setup.

## Methods

The `MipRules` class contains the following methods:

- `$execute()` solves the mixed integer problem.

- `$to_lp()` transforms the object into an `lp_solve` problem object.

- `$is_infeasible` checks whether the current rule system is infeasible.

- `$set_values()` sets observed values and weights (objective function).

## See also

Other Mixed Integer Problem: [`inspect_mip()`](inspect_mip.md)

## Examples

``` r
rules <- validator(x > 1)
mr <- miprules(rules)
mr$to_lp()
#> Model name: errorlocate
#>              x            
#> Minimize     0            
#> V1          -1  <=  -1.001
#> Kind       Std            
#> Type      Real            
#> Upper      Inf            
#> Lower     -Inf            
mr$set_values(c(x=0), weights=c(x=1))
mr$execute()
#> $s
#> [1] 0
#> 
#> $solution
#> [1] TRUE
#> 
#> $values
#>        x .delta_x 
#>    1.001    1.000 
#> 
#> $lp
#> Model name: errorlocate
#>                        x        .delta_x       
#> Minimize               0  1.080750137568       
#> x_ub                   1          -1e+07  <=  0
#> Kind                 Std             Std       
#> Type                Real             Int       
#> Upper                Inf               1       
#> Lower              1.001               0       
#> 
#> $adapt
#>    x 
#> TRUE 
#> 
#> $errors
#>    x 
#> TRUE 
#> 
```
