# Create a mip object from a validator object

Create a mip object from
[`validate::validator()`](https://rdrr.io/pkg/validate/man/validator.html)
object. This is a utility class that translates a validor object into a
mixed integer problem that can be solved. Most users should use
[`locate_errors()`](locate_errors.md) which will handle all translation
and execution automatically. This class is provided so users can
implement or derive an alternative solution.

## Methods

The `MipRules` class contains the following methods:

- `$execute()` calls the mip solver to execute the rules.

- `$to_lp()`: transforms the object into a lp_solve object

- `$is_infeasible` Checks if the current system of mixed integer rules
  is feasible.

- `$set_values`: set values and weights for variables (determines the
  objective function).

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
#> Minimize               0  1.600760886212       
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
