# auxillary functions for simplifying rules

# TRUE | x  -> TRUE
# x | TRUE  -> TRUE
# FALSE | x  -> x
# FALSE | TRUE  -> x
#
# FALSE & x -> FALSE
# x        & FALSE  -> FALSE

# if (TRUE) x  -> x
# if (FALSE) x -> TRUE
