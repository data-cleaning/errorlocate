#' errorlocate
#'
#' Find errors in data given a set of validation rules.
#' The \code{errorlocate} helps to identify obvious errors in datasets.
#' Often raw data contains errors.
#'
#' It works in tandem with the package \code{\link{validate}}.
#' With \code{validate} one can formulate data validation rules to which te data must comply.
#' For example that age cannot be negative.
#' While \code{validate} can identify if a record is valid or not, in general does not identify
#' which of the variables are responsible for the invalidation: a set of validation rules form a web
#' of dependent variables: changing the value of an invalid record to repair for rule 1, may invalidate
#' the record for rule 2.
#'
#' Errorlocate provides a small framework for record based error detection and implements the Felligi Holt
#' algorithm. This algorithm assumes there is no other information available then the values of a record
#' and a set of validation rules.
#' @name errorlocate-package
#' @import methods validate
#' @importFrom stats runif setNames
#' @docType package
NULL
