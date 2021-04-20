#' Convert DCA Object to ggplot
#'
#' @param object dca object created with `dca()`
#' @param type indicates type of plot to produce. must be one of
#' `c("net_benefit", "net_intervention_avoided")`
#' @param smooth Logical indicator whether plot will be smooth with
#' `ggplot2::stat_smooth()`. Default is `FALSE`
#' @param span when `smooth = TRUE`, Controls the amount of smoothing for
#' loess smoother. Smaller numbers produce wigglier lines, larger numbers
#' produce smoother lines. Default is 0.2.
#' @param ... not used
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' dca(cancer ~ cancerpredmarker, data = df_binary) %>%
#'   autoplot()

autoplot.dca <- function(object,
                         type = c("net_benefit", "net_intervention_avoided"),
                         smooth = FALSE,
                         span = 0.2, ...) {
  type <- match.arg(type)

  # specifying smooth or line geom
  ggline <- ifelse(smooth,
                   list(ggplot2::stat_smooth(method = "loess",
                                             se = FALSE,
                                             formula = 'y ~ x',
                                             span = span)),
                   list(ggplot2::geom_line()))

  if (type == "net_benefit") {
  gg <-
    object$dca %>%
    dplyr::filter(!is.na(.data$net_benefit)) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$threshold, y = .data$net_benefit, color = .data$label)) +
    ggline +
    ggplot2::coord_cartesian(ylim = c(object$prevalence * -0.1, object$prevalence)) +
    ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      x = "Threshold Probability",
      y = "Net Benefit",
      color = ""
    )
  }
  else stop("I need to write a plot method for this!")

  gg
}
