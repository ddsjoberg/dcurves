#' Add Standardized Net Benefit
#'
#' Add the standardized net benefit to `dca()` object.
#'
#' @param x object of class `'dca'` calculated with `dca()`
#'
#' @return 'dca' object
#' @export
#' @author Daniel D Sjoberg
#' @seealso [`dca()`], [`net_intervention_avoided()`], [`plot.dca()`], [`as_tibble.dca()`]
#'
#' @examples
#' dca(Surv(ttcancer, cancer) ~ cancerpredmarker, data = df_surv, time = 1) %>%
#'   standardized_net_benefit()
standardized_net_benefit <- function(x) {
  # checking inputs ------------------------------------------------------------
  if (!inherits(x, "dca")) {
    stop("Argument `x=` must be class 'dca' calculated with `dca()`",
         call. = FALSE
    )
  }

  if (sum(x$dca$harm) > 0) {
    stop("Cannot combine model harms with standardized net benefit.", call. = FALSE)
  }

  # add standardized net benefit to the dca tibble -----------------------------
  x$dca <-
    x$dca %>%
    dplyr::mutate(standardized_net_benefit = .data$net_benefit / .data$prevalence)

  x
}
