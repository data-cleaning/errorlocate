# Find errors in data

Find out which fields in a data.frame are "faulty" using validation
rules This method returns found errors, according to the specified
method `x`. Use method [`replace_errors()`](replace_errors.md), to
automatically remove these errors. Use `[base::set.seed()]` beforehand
to make the function call reproducible. \`

## Usage

``` r
locate_errors(
  data,
  x,
  ...,
  cl = NULL,
  Ncpus = getOption("Ncpus", 1),
  timeout = 60
)

# S4 method for class 'data.frame,validator'
locate_errors(
  data,
  x,
  weight = NULL,
  ref = NULL,
  ...,
  cl = NULL,
  Ncpus = getOption("Ncpus", 1),
  timeout = 60
)

# S4 method for class 'data.frame,ErrorLocalizer'
locate_errors(
  data,
  x,
  weight = NULL,
  ref = NULL,
  ...,
  cl = NULL,
  Ncpus = getOption("Ncpus", 1),
  timeout = 60
)
```

## Arguments

- data:

  data to be checked

- x:

  validation rules or errorlocalizer object to be used for finding
  possible errors.

- ...:

  optional parameters that are passed to
  [`lpSolveAPI::lp.control()`](https://rdrr.io/pkg/lpSolveAPI/man/lp.control.html)
  (see details)

- cl:

  optional parallel / cluster.

- Ncpus:

  number of nodes to use. See details

- timeout:

  maximum number of seconds that the localizer should use per record.

- weight:

  `numeric` optional weight specification to be used in the error
  localization (see [`expand_weights()`](expand_weights.md)).

- ref:

  `data.frame` optional reference data to be used in the rules checking

## Value

[`errorlocation-class()`](errorlocation.md) object describing the errors
found.

## Details

Use an `Inf` `weight` specification to fixate variables that can not be
changed. See [`expand_weights()`](expand_weights.md) for more details.

`locate_errors` uses lpSolveAPI to formulate and solves a mixed integer
problem. For details see the vignettes. This solver has many options:
[lpSolveAPI::lp.control.options](https://rdrr.io/pkg/lpSolveAPI/man/lp.control.options.html).
Noteworthy options to be used are:

- `timeout`: restricts the time the solver spends on a record (seconds)

- `break.at.value`: set this to minimum weight + 1 to improve speed.

- `presolve`: default for errorlocate is "rows". Set to "none" when you
  have solutions where all variables are deemed wrong.

`locate_errors` can be run on multiple cores using R package `parallel`.

- The easiest way to use the parallel option is to set `Ncpus` to the
  number of desired cores, @seealso
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html).

- Alternatively one can create a cluster object
  ([`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html))
  and use `cl` to pass the cluster object.

- Or set `cl` to an integer which results in
  [`parallel::mclapply()`](https://rdrr.io/r/parallel/mclapply.html),
  which only works on non-windows.

## See also

Other error finding: [`errorlocation-class`](errorlocation.md),
[`errors_removed()`](errors_removed.md),
[`expand_weights()`](expand_weights.md),
[`replace_errors()`](replace_errors.md)

## Examples

``` r
rules <- validator( profit + cost == turnover
                  , cost >= 0.6 * turnover # cost should be at least 60% of turnover
                  , turnover >= 0 # can not be negative.
                  )
data <- data.frame( profit   = 755
                  , cost     = 125
                  , turnover = 200
                  )

# use set.seed to maake results reproducible
set.seed(42)
le <- locate_errors(data, rules)

print(le)
#> call:  locate_errors(data = data, fh, ref = ref, weight = weight, ...,      cl = cl, Ncpus = Ncpus, timeout = timeout) 
#> located  1  error(s).
#> located  0  missing value(s).
#> Use 'summary', 'values', '$errors' or '$weight', to explore and retrieve the errors.
summary(le)
#> Variable:
#>       name errors missing
#> 1   profit      1       0
#> 2     cost      0       0
#> 3 turnover      0       0
#> Errors per record:
#>   errors records
#> 1      1       1

v_categorical <- validator( branch %in% c("government", "industry")
                          , tax %in% c("none", "VAT")
                          , if (tax == "VAT") branch == "industry"
)

data <- read.csv(text=
"   branch, tax
government, VAT
industry  , VAT
", strip.white = TRUE)
locate_errors(data, v_categorical)$errors
#>      branch   tax
#> [1,]  FALSE  TRUE
#> [2,]  FALSE FALSE

v_logical <- validator( citizen %in% c(TRUE, FALSE)
                      , voted %in% c(TRUE, FALSE)
                      ,  if (voted == TRUE) citizen == TRUE
                      )

data <- data.frame(voted = TRUE, citizen = FALSE)

set.seed(42)
locate_errors(data, v_logical, weight=c(2,1))$errors
#>      voted citizen
#> [1,] FALSE    TRUE

# try a condinational rule
v <- validator( married %in% c(TRUE, FALSE)
              , if (married==TRUE) age >= 17
              )
data <- data.frame( married = TRUE, age = 16)

set.seed(42)
locate_errors(data, v, weight=c(married=1, age=2))$errors
#>      married   age
#> [1,]    TRUE FALSE


# different weights per row
data <- read.csv(text=
"married, age
    TRUE,  16
    TRUE,  14
", strip.white = TRUE)

weight <- read.csv(text=
"married, age
       1,   2
       2,   1
", strip.white = TRUE)

set.seed(42)
locate_errors(data, v, weight = weight)$errors
#>      married   age
#> [1,]    TRUE FALSE
#> [2,]   FALSE  TRUE

# fixate / exclude a variable from error localiziation
# using an Inf weight
weight <- c(age = Inf)

set.seed(42)
locate_errors(data, v, weight = weight)$errors
#>      married   age
#> [1,]    TRUE FALSE
#> [2,]    TRUE FALSE
```
