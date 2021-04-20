#' Convert DCA Object to tibble
#'
#' @param x dca object created with `dca()`
#' @param ... not used
#'
#' @return a tibble
#' @export
#'
#' @examples
#' dca(cancer ~ cancerpredmarker, data = df_binary) %>%
#'   as_tibble()

as_tibble.dca <- function(x, ...) {
  x$dca
}
