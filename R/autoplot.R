#' Export DCA ggplot
#'
#' @param x dca object created with `dca()`
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' dca(cancer ~ cancerpredmarker, data = df_dca) %>%
#'   autoplot()

autoplot.dca <- function(x) {
  x$dca %>%
    ggplot(aes(x = threshold, y = net_benefit, color = variable)) +
    geom_line() +
    ggplot2::coord_cartesian(ylim = c(x$prevalence * -0.1, x$prevalence)) +
    ggplot2::labs(
      x = "Threshold Probability",
      y = "Net Benefit",
      color = ""
    )
}
