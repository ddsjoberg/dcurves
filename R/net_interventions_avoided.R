#' Add Net Interventions Avoided
#'
#' Add the number of net interventions avoided to `dca()` object.
#'
#' @param x object of class `'dca'` calculated with `dca()`
#' @param nper Number to report net interventions per. Default is 100.
#'
#' @return 'dca' object
#' @export
#' @author Daniel D Sjoberg
#' @seealso [`dca()`], [`autoplot.dca()`], [`as_tibble.dca()`]
#'
#' @examples
#' dca(cancer ~ cancerpredmarker, data = df_binary) %>%
#'   net_interventions_avoided()
net_interventions_avoided <- function(x, nper = 100) {
  if (!inherits(x, "dca")) {
    stop("Argument `x=` must be class 'dca' calculated with `dca()`",
      call. = FALSE
    )
  }

  # add net interventions to the dca tibble ------------------------------------
  x$dca <-
    x$dca %>%
    dplyr::left_join(
      dplyr::filter(., .data$variable %in% "all") %>%
        dplyr::select(.data$threshold, net_benefit_all = .data$net_benefit),
      by = "threshold"
    ) %>%
    dplyr::mutate(
      net_intervention_avoided =
        (.data$net_benefit - .data$net_benefit_all) /
          (.data$threshold / (1 - .data$threshold)) * .env$nper
    ) %>%
    dplyr::select(-.data$net_benefit_all)

  # add nper info to dca list --------------------------------------------------
  x$net_interventions_nper <- nper

  # return dca object ----------------------------------------------------------
  x
}
