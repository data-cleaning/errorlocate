# errorlocate 1.1

* For values `> 1e7` in combination with another value `NA` `locate_errors` generated an error. Reported and fixed by Ramon Reinders (issue #36)

* support for `in_range` (issue #33)

# errorlocate 1.0.0

* Fixed a nasty bug in which variable names are mangled by lpsolveAPI, depending
on the seed / added random noise. Thanks to Patrick Driessens

* Added `expand_weights`, which helps in specifying detailed weights for records

* Removed a bug: `Ncpus` was ignored in `replace_errors`

# errorlocate 0.9.9

* Added support for Inf weights, thanks to Guido van den Heuvel.

* Improved default setting of solver `epsd = 1e-12` provides better numerical
stability.

* Too aggressive presolve default for lpSolveAPI (c("rows", "cols")) (issue #34)
, switching back to presolve="rows". Thanks to Sander Scholtus.

# errorlocate 0.9.8

* Added parallel processing options, resulting in speed and memory consumption
improvements. (also for `Ncpus=1`).

* simple ratio's are taken into account and rewritten into linear rules:
`cost/turnover > 0.6` will be rewritten into `cost > 0.6 * turnover`.

* bug fix issue #31: when a record was invalid, but all rules involving the invalidation
contained missing variables, the record was skipped by errorlocate.

* Bug fix for issue #30: when a value >= 1e7 was encountered, all fields were flagged erroneous.
Thanks to Garðar Páll Gíslason.

* Bug fix for log1p, log10 function approximation.

* In long running `locate_error` sessions, the mip solver returned for some records
a numerical instability error code. Seems to be an instability in lpSolve. Resubmitting
same record does return a solution. When a numerical instability in lpSolve is reported
a record is try again. When this fails, it is saved in mps format to the temporary
directory (with a warning).

# errorlocate 0.5.1

* Better name generation for soft linear equality constraints
* Only values of the data.frame that are used in the constraints are added to the matrix.
* Fix for issue #25, when a variable contains only one category. Thanks to @nickforr.
* Fix/warning for issue #27, using an integer variable for categories. Thanks to Jeffrey Hoogland for reporting.
* Fix for handling NA logical values (issue #29).
* Added status and duration info to `errorlocation`. Thanks to Sander Scholtus
* Improved progress bar, showing percentage and taking into account records without errors
* added `inspect_mip` function, allowing for an in depth examination of the mip translation
and execution. Making it easier to debug/find what is wrong with a record / rule set
* experimental functionality for log transformed variables, can be switched on with
`options(errorlocate.allow_log = TRUE)`. This makes it possible to formulate constraints
`total_salary >= min_salary * n_employees` as 
`log(total_salary) >= log(min_salary) + log(n_employees)`.

# errorlocate 0.4

* Implemented optimization, only invalid records are now treated. Can greatly
enhance processing time! Thanks to Jos de Waard.
* Fixed issue #21, thanks to Sander Scholtus: strict equalities
* Fixed issue #22, thanks to Sander Scholtus: missing columns in data.
* Fixed issue #23, "<var> =="" FALSE in if clause was handled incorrectly.

# errorlocate 0.3.0

* Fixed issue #19 and #20: rules now may contain var_group and assignments
* Fixed an issue with soft constraints: type of variables was sometimes incorrect
* Parsing of if statements with more than 2 expressions in the condition is now improved

# errorlocate 0.2.0

* Fixed issue #17: if-rules may contain a linear equality.
