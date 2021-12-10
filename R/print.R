#' Print `dca()` object
#'
#' @param x dca object
#' @param ... not used
#'
#' @return a ggplot
#' @export
#' @keywords internal
#'
#' @examples
#' dca(cancer ~ cancerpredmarker, data = df_binary) %>%
#'   print()
print.dca <- function(x, ...) {
  if ("net_intervention_avoided" %in% names(x$dca)) {
    message("Printing with `plot(x, type = 'net_intervention_avoided', smooth = FALSE, show_ggplot_code = FALSE)`")
    print(plot.dca(x, type = "net_intervention_avoided", smooth = FALSE, show_ggplot_code = FALSE))
  }
  else if ("standardized_net_benefit" %in% names(x$dca)) {
    message("Printing with `plot(x, type = 'standardized_net_benefit', smooth = FALSE, show_ggplot_code = FALSE)`")
    print(plot.dca(x, type = "standardized_net_benefit", smooth = FALSE, show_ggplot_code = FALSE))
  }
  else {
    message("Printing with `plot(x, type = 'net_benefit', smooth = FALSE, show_ggplot_code = FALSE)`")
    print(plot.dca(x, type = "net_benefit", smooth = FALSE, show_ggplot_code = FALSE))
  }
}
