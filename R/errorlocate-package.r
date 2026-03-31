#' Find errors in data given a set of validation rules.
#'
#' `errorlocate` helps identify which cells in a record are likely causing rule
#' violations.
#'
#' The package is designed to work with [validate::validator()] rules. While
#' `validate` can determine whether a record is valid, `errorlocate` tries to
#' identify which fields are most likely erroneous.
#'
#' This is non-trivial because rules are interdependent: changing one value to
#' satisfy one rule may violate another rule.
#'
#' `errorlocate` implements record-level error localization based on the
#' Fellegi-Holt approach. It translates the localization task into a mixed
#' integer optimization problem and uses a MIP solver to find a minimum-weight
#' set of fields to change.
#'
#' Typical workflow:
#' - Define rules with [validate::validator()].
#' - Locate likely errors with [locate_errors()].
#' - Replace flagged cells with missing values using [replace_errors()].
#' @name errorlocate-package
#' @import methods validate
#' @importFrom stats runif setNames
#' @docType package
#' @references
#'  T. De Waal (2003) Processing of Erroneous and Unsafe Data. PhD thesis, University of Rotterdam.
#'
#'  Van der Loo, M., de Jonge, E, Data Cleaning With Applications in R
#'
#'  E. De Jonge and Van der Loo, M. (2012) Error localization as a mixed-integer program in
#'  editrules.
#'
#'  lp_solve and Kjell Konis. (2011). lpSolveAPI: R Interface for
#'  lp_solve version 5.5.2.0. R package version 5.5.2.0-5.
#'  http://CRAN.R-project.org/package=lpSolveAPI
"_PACKAGE"
