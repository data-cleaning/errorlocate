# Find errors in data

## Intro

Errorlocate uses validation rules from package `validate` to locate
faulty values in observations (or in database slang: erronenous *fields*
in *records*).

It follows this simple recipe (Fellegi-Holt):

- Check if a record is valid (using supplied validation rules)
- If not valid then adjust the minimum number of values to make it
  valid.

`errorlocate` does this by translating the validation rules and record
values into a mixed integer problem (see
`vignette("inspect_mip", package="errorlocate"`) and solving it using
the R package `lpSolveAPI`.

## Workflow

Locating errors is part of a data cleaning workflow, see e.g. Loo and
Jonge (2018), where after locating errors, these errors are removed (set
to `NA`) and subsequently imputed. The input for errorlocate are
validation rules, which can be formulated using package `validate` (van
der Loo and de Jonge 2019).

A typical workflow for using `errorlocate` is:

1.  Formulate validation rules using
    [`validate::validator()`](https://rdrr.io/pkg/validate/man/validator.html)
2.  Check which records are invalid using
    [`validate::confront()`](https://rdrr.io/pkg/validate/man/confront.html)
3.  Use
    [`validate::summary()`](https://rdrr.io/pkg/validate/man/validate-summary.html)
    to get an overview of invalid records and the rules they violate.
4.  (Optional) Correct obvious data errors which correction rules, for
    example using `dcmodify::modify`
5.  (Optional) Determine `weights` for variables, indicating how likely
    they are to be faulty. Higher weights indicate more trust in the
    variable.
6.  Use `locate_errors` to find which values are faulty for the invalid
    records.
7.  Use `replace_errors` to set faulty values to `NA`.
8.  Use deductive imputation to fill in missing values that follow from
    the rules, e.g. `deductive::deduImpute()`
9.  Impute the remaining missing values using your favorite imputation
    method, e.g. `simputation`
10. (Optional) Re-validate the data using
    [`validate::confront()`](https://rdrr.io/pkg/validate/man/confront.html)
    to check if all records are now valid.

Post hoc:

- Analyze the output of `locate_errors` to get insights in which
  variables are more often faulty than others, which can be used to
  improve future data collection processes.

## Main methods

`errorlocate` has two main functions to be used:

- `locate_errors` for detecting errors
- `replace_errors` for replacing faulty values with `NA`

``` r
library(validate)
library(errorlocate)
```

Let’s start with a simple example:

We have a rule that age cannot be negative:

``` r
rules <- validator(age > 0)
```

And we have the following data set

``` r
"age, income
 -10,    0  
  15, 2000
  25, 3000
  NA, 1000
" -> csv
d <- read.csv(textConnection(csv), strip.white = TRUE)
```

    #>   age income
    #> 1 -10      0
    #> 2  15   2000
    #> 3  25   3000
    #> 4  NA   1000

``` r
le <- locate_errors(d, rules)
summary(le)
#> Variable:
#>     name errors missing
#> 1    age      1       1
#> 2 income      0       0
#> Errors per record:
#>   errors records
#> 1      0       3
#> 2      1       1
```

`summary(le)` gives an overview of the errors found in this data set.
The complete error listing can be found with:

``` r
le$errors
#>        age income
#> [1,]  TRUE  FALSE
#> [2,] FALSE  FALSE
#> [3,] FALSE  FALSE
#> [4,]    NA  FALSE
```

Which says that record 1 has a faulty value for age.

Suppose we expand our rules

``` r
rules <- validator( r1 = age > 0
                  , r2 = if (income > 0) age > 16
                  )
```

With
[`validate::confront`](https://rdrr.io/pkg/validate/man/confront.html)
we can see that rule `r2` is violated (record 2).

``` r
d |> 
  confront(rules) |> 
  summary()
#>   name items passes fails nNA error warning               expression
#> 1   r1     4      2     1   1 FALSE   FALSE                  age > 0
#> 2   r2     4      2     1   1 FALSE   FALSE income <= 0 | (age > 16)
```

What errors will be found by `locate_errors`?

``` r
set.seed(1)
le <- locate_errors(d, rules)
le$errors
#>        age income
#> [1,]  TRUE  FALSE
#> [2,]  TRUE  FALSE
#> [3,] FALSE  FALSE
#> [4,]    NA  FALSE
```

It now detects that `age` in observation 2 is also faulty, since it
violates the second rule. Note that we use `set.seed`. This is needed
because in this example, either `age` or `income` can be considered
faulty. `set.seed` assures that the procedure is reproducible.

With `replace_errors` we can remove the errors (which still need to be
imputed).

``` r
d_fixed <- replace_errors(d, le)
d_fixed |> confront(rules) |>summary()
#>   name items passes fails nNA error warning               expression
#> 1   r1     4      1     0   3 FALSE   FALSE                  age > 0
#> 2   r2     4      2     0   2 FALSE   FALSE income <= 0 | (age > 16)
```

In which `replace_errors` set all faulty values to `NA`.

``` r
d_fixed
#>   age income
#> 1  NA      0
#> 2  NA   2000
#> 3  25   3000
#> 4  NA   1000
```

### Weights

`locate_errors` allows for supplying weights for the variables. It is
common that the quality of the observed variables differs. When we have
more trust in `age` because it was retrieved from the official
population register, we can give it more weight so it chooses income
when it has to decide between the two (record 2):

``` r
set.seed(1) # good practice, see later in this document
weight <- c(age = 2, income = 1) 
le <- locate_errors(d, rules, weight)
le$errors
#>        age income
#> [1,]  TRUE  FALSE
#> [2,] FALSE   TRUE
#> [3,] FALSE  FALSE
#> [4,]    NA  FALSE
```

Weights can be specified in different ways: (see also
[`errorlocate::expand_weights`](../reference/expand_weights.md)):

- not specifying: all variables will have weight 1
- named `vector`: all records will have same set of weights. Unspecified
  columns will have weight 1.
- named `matrix` or `data.frame`, same dimension as the data: specify
  weights per record.
- Use `Inf` weights to fixate a variable, so it won’t be changed.

## Reproducability (`set.seed`)

The error location procedure can have, in some cases, multiple optimal
solutions. For example given the rule:

- `if (married == TRUE) age > 16`

and the following data:

    #>   age married
    #> 1   4    TRUE

Then either `age` or `married` can be considered faulty. When no weights
are specified, `locate_errors` will randomly choose one of the two. It
does this by adding internally a small amount of random noise to the
weights. To make sure that the results are reproducible, it is good
practice to use `set.seed` before calling `locate_errors`.

Note that using `set.seed` only makes the exact indentical input
reproducible. Whenever the input `data.frame` have a different order,
different number of rows or columns, the generated noise for the weights
of some of the records will be different. This level of reproducibility
can be achieved by generating the noise beforehand using
[`errorlocate::add_noise`](../reference/add_noise.md), storing the
weights together with the records and supplying the noisy weights to
`locate_errors`.

## Performance / Parallelisation

`locate_errors` solves a mixed integer problem. When the number of
interactions between validation rules is large, finding an optimal
solution can become computationally intensive. Both `locate_errors` as
well as `replace_errors` have a parallization option: `Ncpus` making use
of multiple processors. The `$duration` (s) property of each solution
indicates the time spent to find a solution for each record. This can be
restricted using the argument `timeout` (s).

``` r
# restrict time per record to max 30 seconds
le <- locate_errors(d, rules, timeout=30)
# duration is in seconds.
le$duration
#> [1] 0.001874208 0.001541853 0.000000000 0.001425982
```

Loo, Mark van der, and Edwin de Jonge. 2018. *Statistical Data Cleaning
with Applications in* . John Wiley & Sons.

van der Loo, Mark, and Edwin de Jonge. 2019. *Validate: Data Validation
Infrastructure*. <https://CRAN.R-project.org/package=validate>.
