#' Plot DCA Object with ggplot
#'
#' @param object dca object created with `dca()`
#' @param type indicates type of plot to produce. Must be one of
#' `c("net_benefit", "net_intervention_avoided")`. The default is
#' `"net_benefit"`, unless the net interventions have been calculated
#' when `"net_intervention_avoided"` is used.
#' @param smooth Logical indicator whether plot will be smooth with
#' `ggplot2::stat_smooth()`. Default is `FALSE`
#' @param span when `smooth = TRUE`, Controls the amount of smoothing for
#' loess smoother. Smaller numbers produce wigglier lines, larger numbers
#' produce smoother lines. Default is `0.2`.
#' @param ... not used
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' dca(cancer ~ cancerpredmarker, data = df_binary) %>%
#'   autoplot(nper = 100)

autoplot.dca <- function(object,
                         type = NULL,
                         smooth = FALSE,
                         span = 0.2, ...) {
  # set type of figure to create -----------------------------------------------
  if (is.null(type) && !"net_intervention_avoided" %in% names(object$dca))
    type <- "net_benefit"
  else if (is.null(type) && "net_intervention_avoided" %in% names(object$dca))
    type <- "net_intervention_avoided"
  type <- match.arg(type, choices = c("net_benefit", "net_intervention_avoided"))

  if (type %in% "net_intervention_avoided" &&
      !"net_intervention_avoided" %in% names(object$dca))
    paste("Cannot specify `type = 'net_intervention_avoided' without",
          "first running `net_intervention_avoided()`.") %>%
    stop(call. = FALSE)


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
      ggplot2::ggplot(ggplot2::aes(x = .data$threshold,
                                   y = .data$net_benefit,
                                   color = .data$label)) +
      ggline +
      ggplot2::coord_cartesian(ylim = c(object$prevalence * -0.1, object$prevalence)) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(
        x = "Threshold Probability",
        y = "Net Benefit",
        color = ""
      )
  }
  else if (type == "net_intervention_avoided") {
    gg <-
      object$dca %>%
      dplyr::filter(!is.na(.data$net_intervention_avoided),
                    !.data$variable %in% c("all", "none")) %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$threshold,
                                   y = .data$net_intervention_avoided,
                                   color = .data$label)) +
      ggline +
      ggplot2::coord_cartesian(ylim = c(0, object$net_interventions_nper)) +
      ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      ggplot2::labs(
        x = "Threshold Probability",
        y = paste("Net reduction in interventions per",
                  object$net_interventions_nper,
                  "patients"),
        color = ""
      )
  }

  gg
}
