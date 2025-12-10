# inspect the mip problem formulation

Utility function to inspect the mip problem for a record. `inspect_mip`
can be used as a "drop-in" replacement for
[`locate_errors()`](locate_errors.md), but works on the first record.

## Usage

``` r
inspect_mip(data, x, weight, ...)
```

## Arguments

- data:

  data to be checked

- x:

  validation rules or errorlocalizer object to be used for finding
  possible errors.

- weight:

  `numeric` optional weight specification to be used in the error
  localization (see [`expand_weights()`](expand_weights.md)).

- ...:

  optional parameters that are passed to
  [`lpSolveAPI::lp.control()`](https://rdrr.io/pkg/lpSolveAPI/man/lp.control.html)
  (see details)

## Details

It may sometimes be handy to find out what is happening exactly with a
record. See the example section for finding out what to do with
inspect_mip. See [`vignette("inspect_mip")`](../articles/inspect_mip.md)
for more details.

## See also

Other Mixed Integer Problem: [`MipRules-class`](MipRules-class.md)

## Examples

``` r
rules <- validator(x > 1)
data <- list(x = 0)
weight <- c(x = 1)

mip <- inspect_mip(data, rules)
print(mip)
#> Mip rules object:
#>    methods: '$to_lp()', '$execute', '$set_values()'
#>    properties: '$mip_rules', '$objective', '$is_infeasible', '$rules'
#> 
#> Generates the lp program (see ?inspect_mip) 
#> 
#> Model name: errorlocate
#>                        x        .delta_x            
#> Minimize               0  1.195669834734            
#> V1                    -1               0  <=  -1.001
#> x_ub                   1          -1e+07  <=       0
#> x_lb                  -1          -1e+07  <=       0
#> Kind                 Std             Std            
#> Type                Real             Int            
#> Upper                Inf               1            
#> Lower               -Inf               0            

# inspect the lp problem (prior to solving it with lpsolveAPI)
lp <- mip$to_lp()
print(lp)
#> Model name: errorlocate
#>                        x        .delta_x            
#> Minimize               0  1.195669834734            
#> V1                    -1               0  <=  -1.001
#> x_ub                   1          -1e+07  <=       0
#> x_lb                  -1          -1e+07  <=       0
#> Kind                 Std             Std            
#> Type                Real             Int            
#> Upper                Inf               1            
#> Lower               -Inf               0            

# for large problems write the lp problem to disk for inspection
# lpSolveAPI::write.lp(lp, "my_problem.lp")

# solve the mip system / find a solution
res <- mip$execute()
names(res)
#> [1] "s"        "solution" "values"   "lp"       "adapt"    "errors"  

# lpSolveAPI status of finding a solution
res$s
#> [1] 0

# lp problem after solving (often simplified version of first lp)
res$lp
#> Model name: errorlocate
#>                        x        .delta_x       
#> Minimize               0  1.195669834734       
#> x_ub                   1          -1e+07  <=  0
#> Kind                 Std             Std       
#> Type                Real             Int       
#> Upper                Inf               1       
#> Lower              1.001               0       

# records that are deemed "faulty"
res$errors
#>    x 
#> TRUE 

# values of variables used in the mip formulation. Also contains a valid solution
# for "faulty" variables
res$values
#>        x .delta_x 
#>    1.001    1.000 

# see the derived mip rules and objective function, used in the construction of
# lp problem
mip$mip_rules()
#> [[1]]
#> V1: -x < -1
#> [[2]]
#> x_ub: x - 1e+07*.delta_x <= 0
#> [[3]]
#> x_lb: -x - 1e+07*.delta_x <= 0
mip$objective
#> .delta_x 
#>  1.19567 
```
