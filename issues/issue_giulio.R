#' ---
#' title:
#' output:
#'   pdf_document: default
#' ---
#'
#' Loading the data:
#'
## ---- include=FALSE------------------------------------------------------
knitr::opts_chunk$set(comment = NA, warning = TRUE)

#'
#'
## ------------------------------------------------------------------------
library(validate)
library(errorlocate)

v <- validator(
  region                 = region %in% c("Africa", "Americas", "Asia", "Europe", "Oceania"),
  fertility_ratio        = TFR/GDP <= 0.038,
  contraception_ratio    = contraception/GDP <= 0.143,
  infant_mortality_ratio = infant.mortality/GDP <= 0.705
)

nations <- read.table("https://socialsciences.mcmaster.ca/jfox/Books/RCommander/Nations.txt")

#'
#' Lets confront:
#'
## ------------------------------------------------------------------------
cn <- confront(nations, v)
summary(cn)

#'
#' Let's find errors:
#'
## ---- warning=TRUE-------------------------------------------------------
le <- locate_errors(nations, v)

#' But:
## ------------------------------------------------------------------------
le # shows no errors!

#'
#' And this is because the last three rules are ignored by locate_errors: they are not
#' linear! `locate_errors` should create a warning saying so. This warning is sometimes shown and sometimes not (do not yet know why).
#'
#' ## Fix
#'
#' We can fix the issue by changing te rules into linear rules
## ------------------------------------------------------------------------
v2 <- validator(
  region                 = region %in% c("Africa", "Americas", "Asia", "Europe", "Oceania"),
  fertility_ratio        = TFR <= 0.038 * GDP,
  contraception_ratio    = contraception <= 0.143 * GDP,
  infant_mortality_ratio = infant.mortality <= 0.705 * GDP
)

le <- locate_errors(nations, v2)
le$errors
#'
#' And now it works as  expected.
