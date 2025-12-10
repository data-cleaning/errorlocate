# Inspecting the errorlocate Mixed Integer Program

``` r
library(errorlocate)
#> Loading required package: validate
```

## Intro

Errorlocate uses the linear, categorical and conditional rules from a
rules set formulated with R package
[`validate`](https://cran.r-project.org/package=validate), to create a
Mixed Integer Problem.

For most users the details of the translation are not relevant and
hidden in `locate_errors`. Often the number of errors found and the
processing time are much more relevant parameters.

In a few cases, you may run into a problems with your error localization
problem:

1.  The processing time of (some of the records) of `locate_errors` is
    high.
2.  `locate_errors` missed an obvious error.
3.  `locate_errors` indicates that it did not find a valid solution (for
    some records) .

Problem a. can be addressed by using the parallel argument of
`locate_errors` (and `replace_errors`). Problem b can be due to that
`error_locate` ignores non-linear rules, and therefore is not able to
deduce the errors, because it only takes linear, categorical and
conditional rules into a account.

There may also be problems with your rules set. Problems set may be
mitigated by using the
[`validatetools`](https://cran.r-project.org/package=validate) package
that can detect conflicting and redundant rules and has methods to
simplify your rule set.

If you want to dive deep into the mixed integer problem that is created
by `error_locate` you can use the `inspect_mip` function.

## A bit of theory

In the following sections an example is given of how linear, categorical
and conditional rules are written as Mixed Integer Problems. First let’s
see how these rules in validator can be formally defined.

## Formal description

### Rule $r_{i}(\mathbf{x})$

Each translatable rule $r_{i}(\mathbf{x})$ can be written as a
disjunction of atomic clauses $C_{i}^{j}(x)$: it is a function $r_{i}$
that operates on (some of) the values of record
$\mathbf{x} = \left( x_{1},\ldots,x_{n} \right)$ and is `TRUE` (valid)
or `FALSE` (not valid)

$$r_{i}(\mathbf{x}) = \bigvee\limits_{j}C_{i}^{j}(\mathbf{x})$$

with each atomic clause:

$$C_{i}^{j}(\mathbf{x}) = \left\{ \begin{array}{l}
{\mathbf{a}^{T}\mathbf{x} \leq b} \\
{\mathbf{a}^{T}\mathbf{x} = b} \\
{x_{j} \in F_{ij}{\text{with}\mspace{6mu}}F_{ij} \subseteq D_{j}} \\
{x_{j} \notin F_{ij}{\text{with}\mspace{6mu}}F_{ij} \subseteq D_{j}} \\

\end{array} \right.$$

Each linear, categorical or conditional rule $r_{i}$ can be written in
this form.

### Example 1

``` r
rules <- validator(example_1 = if (income > 0) age >= 16)
rules$exprs()
#> $example_1
#> income <= 0 | (age - 16 >= -1e-08)
#> attr(,"reference")
#> example_1 
#>         1
```

So the rule `if (income > 0) age >= 16` can be written as (`income <= 0`
OR `age >=16`)

### Example 2

``` r
rules <- validator(example_2 = if (has_house == "yes") income >= 1000)
rules$exprs()
#> $example_2
#> has_house != "yes" | (income - 1000 >= -1e-08)
#> attr(,"reference")
#> example_2 
#>         1
```

So the rule `if (has_house == "yes") income >= 1000)` can be written as
(`has_house != "yes"` OR `age >=1000`)

## Rule system:

The rules form a system $R(\mathbf{x})$:

$$R(\mathbf{x}) = \bigwedge\limits_{i}r_{i}$$ which means that all rules
$r_{i}$ must be valid. If $R(\mathbf{x})$ is true for record
$\mathbf{x}$, then the record is valid, otherwise one (or more) of the
rules is violated.

## Mixed Integer Programming to FH

Each rule set $R(\mathbf{x})$ can be translated into a mip problem and
solved.

$$\begin{array}{r}
{{\text{Minimize}\mspace{6mu}}f(\mathbf{x}) = 0;} \\
{{\text{s.t.}\mspace{6mu}}{\mathbf{R}\mathbf{x}} \leq \mathbf{d}} \\

\end{array}$$ - $f(\mathbf{x})$ is the (weighted) number of changed
variable: $\delta_{i} \in {0,1}$

$$f(\mathbf{x}) = \sum\limits_{i = 1}^{N}w_{i}\delta_{i}$$

- $\mathbf{R}$ contains rules:

  - $\mathbf{R}_{H}(\mathbf{x}) \leq \mathbf{d}_{H}$ that were specified
    with `validate`/`validator`

  - $\mathbf{R}_{0}(\mathbf{x},{\mathbf{δ}}) \leq \mathbf{d}_{0}$ : soft
    constraints that try fix the current record of $\mathbf{x}$ to the
    observed values.

## Enter `inspect_mip`:

### Linear rules

Most users will use the function `locate_errors` to find errors. The
function `inspect_mip` works exactly same, except that it operates on
just one record in stead of a whole `data.frame`. The result of
`inspect_mip` is a mip object, that is not yet executed and can be
inspected.

``` r
rules <- validator( r1 = age >= 18
                  , r2 = income >= 0
                  )
data <- data.frame(age = c(12, 35), income = c(2000, -1000))
data
#>   age income
#> 1  12   2000
#> 2  35  -1000
```

So we detect two errors in the dataset:

``` r
summary(confront(data, rules))
#>   name items passes fails nNA error warning           expression
#> 1   r1     2      1     1   0 FALSE   FALSE   age - 18 >= -1e-08
#> 2   r2     2      1     1   0 FALSE   FALSE income - 0 >= -1e-08
```

``` r
locate_errors(data, rules)$errors
#>        age income
#> [1,]  TRUE  FALSE
#> [2,] FALSE   TRUE
```

Lets inspect the first record

``` r
mip <- inspect_mip(data, rules)
#> Warning: Taking record 1, ignoring rest of the records...
```

The `mip` object contains the mip problem before it is executed. We can
inspect the lp problem, prior to solving it with `lpSolveApi` with

``` r
# inspect the lp problem (prior to solving it with lpsolveAPI)
lp <- mip$to_lp()
print(lp)
```

      Model name: errorlocate
                            age          income      .delta_age   .delta_income           
      Minimize                0               0  1.003699720604  1.233196748654           
      r1                     -1               0               0               0  <=    -18
      r2                      0              -1               0               0  <=      0
      age_ub                  1               0          -1e+07               0  <=     12
      income_ub               0               1               0          -1e+07  <=   2000
      age_lb                 -1               0          -1e+07               0  <=    -12
      income_lb               0              -1               0          -1e+07  <=  -2000
      Kind                  Std             Std             Std             Std           
      Type                 Real            Real             Int             Int           
      Upper                 Inf             Inf               1               1           
      Lower                -Inf            -Inf               0               0

Validator rules `r1` and `r2` are encoded in two lines of the model. The
values of the current record are encoded as soft constraints in
`age_ub`, `age_lb`, `income_lb` and `income_ub`. These constraints try
to fix the values of `age` at 12 and `income` at 2000, but can be
violated, setting `.delta_age` or `.delta_income` to 1.

For large problems the lp problem can be written to disk for inspection

``` r
mip$write_lp("my_problem.lp")
```

Once we execute the mip project, the lp solver is executed on the
problem:

``` r
res <- mip$execute()
```

Extra arguments are passed through to `lpSolveAPI`. The result object
contains several properties:

``` r
names(res)
#> [1] "s"        "solution" "values"   "lp"       "adapt"    "errors"
```

`res$solution` indicates of a solution was found

``` r
res$solution
#> [1] TRUE
```

`res$s` indicates the `lpSolveAPI` status, what kind of solution was
found.

``` r
res$s
#> [1] 0
```

`res$errors` indicates which fields/values are deemed erroneous:

``` r
res$errors
#>    age income 
#>   TRUE  FALSE
```

`res$values` contains the values for the valid solution that has been
found by the lpsolver:

``` r
res$values
#>           age        income    .delta_age .delta_income 
#>            18          2000             1             0
```

Note that the solver has found that setting `age` from 12 to 18 gives a
valid solution. `.delta_age = 1` indicates that `age` contained an
error.

The result object `res` also contains an `lp` object after optimization.
This object can be further investigated using `lpSolveAPI` functions.

``` r
# lp problem after solving
res$lp
```

      Model name: errorlocate
                            age          income      .delta_age   .delta_income           
      Minimize                0               0  1.003699720604  1.233196748654           
      age_ub                  1               0          -1e+07               0  <=     12
      income_ub               0               1               0          -1e+07  <=   2000
      income_lb               0              -1               0          -1e+07  <=  -2000
      Kind                  Std             Std             Std             Std           
      Type                 Real            Real             Int             Int           
      Upper                 Inf             Inf               1               1           
      Lower                  18               0               0               0

Note that the lp problem has been simplified. For example the single
variable constraints,the lp problem/object after solving shows that the
solver has optimized some of the rules: it has moved rule `r1` and `r2`
into the `Lower boundary` conditions. It also removed `age_lb` because
that was superfluous with respect to the boundary conditions.

### Categorical rule

In categorical rules, each category is coded in a separate column/mip
variable: e.g. if we have a `working` variable, with two categories
(“job”, “retired”), the mip problem is encoded as follows:

``` r
rules <- validator( r1 = working %in% c("job","retired")                  )
data <- data.frame(working="?")
```

| working |
|:--------|
| ?       |

``` r
mip <- inspect_mip(data, rules)
mip$to_lp()
```

      Model name: errorlocate
                      working:?      working:job  working:retired   .delta_working      
      Minimize                0                0                0   1.289767244598      
      r1                      0                1                1                0  =  1
      working                 1                0                0                1  =  1
      Kind                  SOS              SOS              SOS              Std      
      Type                  Int              Int              Int              Int      
      Upper                   1                1                1                1      
      Lower                   0                0                0                0

Row `r1` indicates that either `working:job` or `working:retired` must
be true. The `Kind` row (`SOS`) indicates that these variables share the
same switch, only one of them can be set.

``` r
mip$execute()$values
#>       working:?     working:job working:retired  .delta_working 
#>               0               1               0               1
```

#### Multiple categories:

With categorical variables it is also possible to specify `if-then`
rules. These are encoded as one mip rule:

``` r
rules <- validator( r1 = if (voted == TRUE) adult == TRUE)
data <- data.frame(voted = TRUE, adult = FALSE)
```

| voted | adult |
|:------|:------|
| TRUE  | FALSE |

``` r
mip <- inspect_mip(data, rules)
mip$to_lp()
```

      Model name: errorlocate
                         adult           voted    .delta_adult    .delta_voted       
      Minimize               0               0  1.087470313418  1.437300330378       
      r1                    -1               1               0               0  <=  0
      voted                  0               1               0               1   =  1
      adult                 -1               0               1               0   =  0
      Kind                 Std             Std             Std             Std       
      Type                 Int             Int             Int             Int       
      Upper                  1               1               1               1       
      Lower                  0               0               0               0

``` r
mip$execute()$values
#>        adult        voted .delta_adult .delta_voted 
#>            1            1            1            0
```

### Conditional rule

``` r
rules <- validator( r1 = if (income > 0) age >= 16)
data <- data.frame(age = 12, income = 2000)
```

| age | income |
|----:|-------:|
|  12 |   2000 |

``` r
mip <- inspect_mip(data, rules)
```

`errorlocate` encodes this rule into multiple rules (as noted in the
theoretical section above), so rule `r1` is chopped into 1 rule + 2 sub
rules:

`r1: if (income > 0) age >= 16`:

- `r1._lin1: if (r1._lin1 == FALSE) income <= 0`

- `r1._lin2: if (r1._lin2 == FALSE) age >= 16`

- `r1: r1._lin1 == FALSE | r1._lin2 == FALSE`

This can be seen with:

``` r
mip$mip_rules()
#> [[1]]
#> r1: r1._lin1 + r1._lin2 <= 1
#> [[2]]
#> r1._lin1: income - 1e+07*r1._lin1 <= 0
#> [[3]]
#> r1._lin2: -age - 1e+07*r1._lin2 <= -16
#> [[4]]
#> income_ub: income - 1e+07*.delta_income <= 2000
#> [[5]]
#> age_ub: age - 1e+07*.delta_age <= 12
#> [[6]]
#> income_lb: -income - 1e+07*.delta_income <= -2000
#> [[7]]
#> age_lb: -age - 1e+07*.delta_age <= -12
```

The resulting lp model is:

``` r
mip$to_lp()
```

      Model name: errorlocate
                            age          income      .delta_age   .delta_income        r1._lin1        r1._lin2           
      Minimize                0               0  1.160192865412  1.017120666336               0               0           
      r1                      0               0               0               0               1               1  <=      1
      r1._lin1                0               1               0               0          -1e+07               0  <=      0
      r1._lin2               -1               0               0               0               0          -1e+07  <=    -16
      income_ub               0               1               0          -1e+07               0               0  <=   2000
      age_ub                  1               0          -1e+07               0               0               0  <=     12
      income_lb               0              -1               0          -1e+07               0               0  <=  -2000
      age_lb                 -1               0          -1e+07               0               0               0  <=    -12
      Kind                  Std             Std             Std             Std             Std             Std           
      Type                 Real            Real             Int             Int             Int             Int           
      Upper                 Inf             Inf               1               1               1               1           
      Lower                -Inf            -Inf               0               0               0               0

``` r
mip$execute()$values
#>           age        income    .delta_age .delta_income      r1._lin1 
#>            12             0             0             1             0 
#>      r1._lin2 
#>             1
```

### Alltogether:

This works together with categorical, linear and conditional rules.

``` r
rules <- validator( r1 = working %in% c("no_job", "job","retired")
                  , r2 = if (age < 12) working == "no_job"
                  , r3 = if (working == "retired") age > 50
                  , r4 = age >=0
                  )

data <- data.frame(age = 12, working="retired")
mip <- inspect_mip(data, rules)
mip$execute()$errors
#> working     age 
#>   FALSE    TRUE
```

### Weights

The weights for each variable are normally set to 1, and `errorlocate`
adds some random remainder to the weights: so the solutions are unique
and reproducible (using `set.seed`).

``` r
set.seed(42)
rules <- validator( r1 = if (voted == TRUE) adult == TRUE)
data <- data.frame(voted = TRUE, adult = FALSE)
mip <- inspect_mip(data, rules, weight = c(voted = 3, adult=1))
```

`$objective` contains the generated weights:

``` r
mip$objective
#> .delta_voted .delta_adult 
#>     3.457403     1.468538
```

These are assigned to the `delta` variables in the objective function of
the mip.

``` r
mip$to_lp()
```

      Model name: errorlocate
                         adult           voted    .delta_adult    .delta_voted       
      Minimize               0               0  1.468537706648  3.457403021748       
      r1                    -1               1               0               0  <=  0
      voted                  0               1               0               1   =  1
      adult                 -1               0               1               0   =  0
      Kind                 Std             Std             Std             Std       
      Type                 Int             Int             Int             Int       
      Upper                  1               1               1               1       
      Lower                  0               0               0               0
