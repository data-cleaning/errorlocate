#' ---
#' title: "parallel errorlocate"
#' author: "Edwin de Jonge"
#' output: pdf_document
#' ---
#'
## ----setup, include=FALSE-----------------------------------------------------
library(ggplot2)
library(errorlocate)

#'
#' ## Intro
#'
#' The default execution of errorlocate is sequential. Since solving the mip problem
#' can be time consuming, the total calculation time can be considerable.
#'
#' We implemented two parallel methods in `errorlocate` using the `parallel` package.
#' Both methods have their merits.
#'
#' ## Setting up test data
#'
#' Let's first setup some test data:
#'
## -----------------------------------------------------------------------------
library(errorlocate)
rules <- validator( profit + cost == turnover
                    , cost >= 0.6 * turnover # cost should be at least 60% of turnover
                    , turnover >= 0 # can not be negative.
)

set.seed(1)
N <- 5000

turnover <- as.integer(rlnorm(N, log(2e5)))
cost <- as.integer(runif(N, min=0.6, max=1.1) * turnover)
profit <- as.integer(turnover - cost + rnorm(N, 10))
data <- data.frame(profit, cost, turnover)

#'
#' Note that this data set is just for demo purposes and is not realistic: every
#' record is invalid in the same manner, so the calculation time for each record
#' is similar, this is not typical: in many real world scenario's some records have
#' more complex errors then others. For these scenario's the cluster scenario should be
#' used.
#'
#' ## Example
#'
#' ### Sequential
#'
#' The default execution method is sequential: each (invalid) record is check after
#' one another.
#'
#' When executed interactively a progress bar will be shown. Not that this has
#' overhead (about 10% slower than non-interactively).
#'
## -----------------------------------------------------------------------------
# 1 Sequential
time_seq <- system.time({
  le <- locate_errors(data, rules, weight = c(profit=1, cost = 2, turnover = 2))
})
print(time_seq)

#'
#' ### Forking (non Windows)
#'
#' On non-windows systems it is possible to use the forking functionality of
#' `parallel`: `errorlocate` uses the `mclappy`
#'
## -----------------------------------------------------------------------------
# 2 Forking / mclapply
# Just supply a number of cores. (in this case 4)
message("Forking:")
time_fork <- system.time({
  le <- locate_errors(data, rules, weight = c(profit=1, cost = 2, turnover = 2), cl=4)
})
print(time_fork)

#'
#' This gives speed up in this case of about a factor 3.
#'
#' ### Cluster
#'
#' Another option is to let `parallel` create different R sessions and spread the
#' work of finding the errors by using `parLapplyLB`. This has the benefits of that
#' a difficult record that uses the maximum number of seconds (specified with `timeout`)
#' blocks the calculation for all records.
#' In that case other records will stream through the other R sessions.
#'
## -----------------------------------------------------------------------------
# 3a Cluster, load balancing (parLapplyLB)
# Create a cluster
message("Cluster PSOCK:")
cl <- parallel::makeCluster(4)

time_psock <- system.time({
  le <- locate_errors(data, rules, weight = c(profit=1, cost = 2, turnover = 2), cl=4)
})
print(time_psock)

parallel::stopCluster(cl)

#'
#'
## -----------------------------------------------------------------------------
# 3b Cluster, load balancing (parLapplyLB)
# Create a cluster
message("Cluster Fork:")
cl <- parallel::makeForkCluster(4)

time_pfork <- system.time({
  le <- locate_errors(data, rules, weight = c(profit=1, cost = 2, turnover = 2), cl=4)
})
print(time_pfork)

parallel::stopCluster(cl)

#'
#'
#' ##  Compare
#'
## ---- echo =FALSE-------------------------------------------------------------
time_comp <-
  data.frame( type = c("sequential", "fork", "psock", "psock_fork")
            , time = c(time_seq[3],time_fork[3],time_psock[3],time_pfork[3])
            )
ggplot(time_comp, aes(x = type, y = time, fill=type)) +
  geom_col() +
  labs(title="errorlocate time for 5000 records, 4 cores (in seconds)"
      , y = ""
      , fill=""
      )
