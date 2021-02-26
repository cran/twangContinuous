#' Compute the balance table.
#'
#' `bal.table` is a generic function for extracting balance tables from
#'  `ps.cont` objects, one for an unweighted analysis and one for the weighted
#'  analysis.
#'
#' @param x A `ps.cont` object
#' @param digits Number of digits to round to. Default: 3
#' @param ... Additional arguments.
#'
#' @return Returns a data frame containing the balance information.
#'     * `unw` The unweighted correlation between the exposure and each covariate.
#'     * `wcor` The weighted correlation between the exposure and each covariate.
#'
#' @examples
#'   \dontrun{bal.table(test.mod)}
#'
#' @seealso [ps.cont]
#'
#' @export

bal.table <-
  function(x, digits = 3, ...) {
    unw <- round(x$desc$unw$bal.tab$results, digits)
    wcor <- round(x$desc$AAC$bal.tab$results, digits)
    bal.tab <- cbind(unw, wcor)
    colnames(bal.tab) <- c("unw", "wcor")
    return(bal.tab)
  }
