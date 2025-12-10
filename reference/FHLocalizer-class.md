# Fellegi-Holt Errorlocalizer

Implementation of the Fellegi-Holt algorithm using the `ErrorLocalizer`
base class. Given a set of validation rules and a dataset the
Fellegi-Holt algorithm finds for each record the smallest (weighted)
combination of variables that are erroneous (if any).

## Note

Most users do not need this class and can use
[`locate_errors()`](locate_errors.md).

`errorlocalizer` implements Fellegi holt using a MIP-solver. For
problems in which coefficients of the validation rules or the data are
too different, you should consider scaling the data.
