#' Displays a useful description of a `ps.cont` object.
#'
#' Computes a short summary table describing the size of the dataset and the
#' quality of the propensity score weights about a stored `ps.cont` object.
#'
#' @param object A `ps.cont` object
#' @param ... Additional arguments.
#'
#' @return
#'  *`n` The number of subjects.
#'  *`ess` The effective sample size.
#'  *`max.wcor` The largest weighted correlation across the covariates.
#'  *`mean.wcor` The average weighted correlation across the covariates.
#'  *`rms.wcor` The root mean square of the absolute weighted correlations across
#'        the covariates.
#'  *`iter` The estimated optimal number of [gbm] iterations to optimize the
#'        loss function.
#'
#' @examples
#'    \dontrun{summary(test.mod)}
#'
#' @seealso [ps.cont]
#'
#' @method summary ps.cont
#' @export

summary.ps.cont <- function(object,...){
  summary.names <- c("n", "ess", "max.wcor", "mean.wcor", "rms.wcor", "n.trees")

  summary.tab.list <- lapply(object$desc, function(x) {
    do.call("c", x[summary.names])
  })

  summary.tab <- do.call("rbind", summary.tab.list)

  colnames(summary.tab)[colnames(summary.tab) == "n.trees"] <- "iter"

  return(summary.tab)
}
