#' Print `dca()` object
#'
#' @param x dca object
#' @param ... not used
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' dca(cancer ~ cancerpredmarker, data = df_dca) %>%
#'   print()

print.dca <- function(x, ...) {
  message("Printing `dca()` object with `autoplot()`")
  autoplot(x)
}
