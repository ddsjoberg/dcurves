#' Convert DCA Object to tibble
#'
#' @param x dca object created with `dca()`
#' @param ... not used
#'
#' @return a tibble
#' @export
#' @author Daniel D Sjoberg
#' @seealso [`dca()`], [`net_intervention_avoided()`], [`standardized_net_benefit()`], [`plot.dca()`]
#'
#' @examples
#' dca(cancer ~ cancerpredmarker, data = df_binary) %>%
#'   as_tibble()
as_tibble.dca <- function(x, ...) {
  x$dca
}
