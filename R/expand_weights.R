#' Create a weight matrix
#'
#' Expands a weight specification into a weight matrix to be used
#' by `locate_errors` and `replace_errors`. Weights allow for "guiding" the
#' errorlocalization process, so that less reliable values/variables with less
#' weight are selected first. See details on the specification.
#'
#' If weight fine tuning is needed,
#' a possible scenario is to generate a weight `data.frame` using `expand_weights` and
#' adjust it before executing [locate_errors()] or [replace_errors()].
#' The following specifications for `weight` are supported:
#'
#' - `NULL`: generates a weight matrix with `1`'s
#' - a named `numeric`, unmentioned columns will have weight 1
#' - a unnamed `numeric` with a length equal to `ncol(dat)`
#' - a `data.frame` with same number of rows as `dat`
#' - a `matrix` with same number of rows as `dat`
#' - `Inf`, `NA` weights will be interpreted as that those variables must not be
#' changed and are fixated. `Inf` weights perform much better than setting a weight
#' to a large number.
#' @export
#' @param dat `data.frame` the data to be checked
#' @param weight weight specification, see details.
#' @param as.data.frame if `TRUE` a `data.frame` will be returned.
#' @param ... unused
#' @return `matrix` or `data.frame` of same dimensions as `dat`
#' @family error finding
#' @example ./examples/expand_weights.R
expand_weights <- function(dat, weight = NULL, as.data.frame = FALSE, ...){
  nr_rows <- nrow(dat)
  nr_cols <- ncol(dat)

  nms <- names(dat)

  if (length(weight) == 0){
    W <- matrix(1, nrow=nr_rows, ncol=nr_cols, dimnames = list(NULL, nms))
  } else if (is.null(dim(weight))){
    # this is a numeric
    if (!is.null(names(weight))){
      #TODO check for invalid weight names...
      nms_invalid <- !(names(weight) %in% nms)
      if (any(nms_invalid)){
        nms_i <- paste0("'",names(weight)[nms_invalid],"'", collapse = ", ")
        warning("Ignoring weights ", nms_i," (not in the data)", call. = FALSE)
      }
      weight <- weight[nms]
      weight[is.na(weight)] <- 1
    } else if (length(weight) == nr_cols){
      names(weight) <- nms
    } else {
      stop("Invalid weight length. See `?expand_weights`", call. = FALSE)
    }
    W <- t(matrix(weight, nrow=nr_cols, ncol=nr_rows, dimnames = list(nms,NULL)))
  } else {
      if (nrow(weight) != nr_rows){
        stop("A weight matrix has to have the same number rows as the data.
             \nSee `?expand_weights`"
            , call.=FALSE)
      }
      W <- as.matrix(weight)
      if (is.null(colnames(W))){
        if (ncol(weight) != nr_cols){
          stop("A weight matrix has to have the same number columns as the data.
               \nSee `?expand_weights`"
               , call.=FALSE)
        }
        colnames(W) <- nms
      } else {
        nms_invalid <- !(colnames(weight) %in% nms)
        if (any(nms_invalid)){
          nms_i <- paste0("'",colnames(weight)[nms_invalid],"'", collapse = ", ")
          warning("Ignoring weights ", nms_i," (not in the data)", call. = FALSE)
        }
        nms_missing <- !(nms %in% colnames(weight))
        if (any(nms_missing)){
          l <- rep(1, sum(nms_missing))
          names(l) <- nms[nms_missing]
          W <- do.call(cbind, c(list(W), l))
        }
        W <- W[, nms, drop=FALSE]
      }
  }

  if (isTRUE(as.data.frame)){
    return(as.data.frame(W))
  }
  W
}
