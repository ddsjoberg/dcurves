#' Export DCA ggplot
#'
#' @param object dca object created with `dca()`
#' @param type indicates type of plot to produce. must be one of
#' `c("net_benefit", "net_intervention_avoided")`
#' @param ... not used
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' dca(cancer ~ cancerpredmarker, data = df_dca) %>%
#'   autoplot()

autoplot.dca <- function(object, type = c("net_benefit", "net_intervention_avoided"), ...) {
  type <- match.arg(type)

  if (type == "net_benefit") {
  plot <-
    object$dca %>%
    ggplot(aes(x = .data$threshold, y = .data$net_benefit, color = .data$label)) +
    geom_line() +
    ggplot2::coord_cartesian(ylim = c(object$prevalence * -0.1, object$prevalence)) +
    ggplot2::labs(
      x = "Threshold Probability",
      y = "Net Benefit",
      color = ""
    )
  }
  else stop("I need to write a plot method for this!")

  plot
}
