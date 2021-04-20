#' Print `dca()` object
#'
#' @param x dca object
#' @param ... not used
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' dca(cancer ~ cancerpredmarker, data = df_binary) %>%
#'   print()

print.dca <- function(x, ...) {
  message("Printing with `autoplot(x, type = 'net_benefit', smooth = FALSE)`")
  print(autoplot.dca(x))
}
