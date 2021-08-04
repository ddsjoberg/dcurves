#' Plot DCA Object with ggplot
#'
#' @param x dca object created with `dca()`
#' @param type indicates type of plot to produce. Must be one of
#' `c("net_benefit", "net_intervention_avoided", "standardized_net_benefit")`.
#' The default is
#' `"net_benefit"`, unless the net intervention has been calculated
#' when `"net_intervention_avoided"` is used, or if `"standardized_net_benefit"`
#' has been calculated.
#' @param smooth Logical indicator whether plot will be smooth with
#' `ggplot2::stat_smooth()`. Default is `FALSE`
#' @param span when `smooth = TRUE`, Controls the amount of smoothing for
#' loess smoother. Smaller numbers produce wigglier lines, larger numbers
#' produce smoother lines. Default is `0.2`.
#' @param style Must be one of `c("color", "bw")`. Default is `"color"`, and
#' `"bw"` will print a black and white figure
#' @param show_ggplot_code Logical indicating whether to print ggplot2 code used to
#' create figure. Default is `FALSE`. Set to `TRUE` to perform advanced figure
#' customization
#' @param ... not used
#'
#' @return a ggplot2 object
#' @export
#' @author Daniel D Sjoberg
#' @seealso [`dca()`], [`net_intervention_avoided()`], [`standardized_net_benefit()`], [`as_tibble.dca()`]
#'
#' @examples
#' dca(cancer ~ cancerpredmarker, data = df_binary) %>%
#'   plot(smooth = TRUE, show_ggplot_code = TRUE)
plot.dca <- function(x,
                         type = NULL,
                         smooth = FALSE,
                         span = 0.2,
                         style = c("color", "bw"),
                         show_ggplot_code = FALSE, ...) {
  # set type of figure to create -----------------------------------------------
  if (is.null(type) && "net_intervention_avoided" %in% names(x$dca)) {
    type <- "net_intervention_avoided"
  }
  else if (is.null(type) && "standardized_net_benefit" %in% names(x$dca)) {
    type <- "standardized_net_benefit"
  }
  else if (is.null(type)) {
    type <- "net_benefit"
  }

  type <- match.arg(type, choices = c("net_benefit",
                                      "net_intervention_avoided",
                                      "standardized_net_benefit"))
  style <- match.arg(style)

  if (type %in% "net_intervention_avoided" &&
      !"net_intervention_avoided" %in% names(x$dca)) {
    paste(
      "Cannot specify `type = 'net_intervention_avoided' without",
      "first running `net_intervention_avoided()`."
    ) %>%
      stop(call. = FALSE)
  }
  if (type %in% "standardized_net_benefit" &&
      !"standardized_net_benefit" %in% names(x$dca)) {
    paste(
      "Cannot specify `type = 'standardized_net_benefit' without",
      "first running `standardized_net_benefit()`."
    ) %>%
      stop(call. = FALSE)
  }

  # data prep expressions ------------------------------------------------------
  expr_data_prep <-
    list(
      expr(as_tibble(x)),
      switch(type,
             "net_benefit" = expr(dplyr::filter(!is.na(!!sym("net_benefit")))),
             "standardized_net_benefit" = expr(dplyr::filter(!is.na(!!sym("standardized_net_benefit")))),
             "net_intervention_avoided" =
               expr(dplyr::filter(!is.na(!!sym("net_intervention_avoided")),
                                  !(!!sym("variable") %in% c("all", "none")))))
    )

  # assign aes() and geom_*() arguments ----------------------------------------
  aes.args <-
    list(x = expr(!!sym("threshold")), y = expr(!!sym(type))) %>%
    c(switch( style,
              "bw" = list(linetype = expr(!!sym("label"))),
              "color" = list(color = expr(!!sym("label")))))
  geom.args <-
    switch(
      smooth,
      list(method = "loess", se = FALSE, formula = "y ~ x", span = inject(!!span))
    ) %||%
    list(NULL) %>%
    c(switch(style == "bw", list(color = "black")))

  # build full ggplot expression -----------------------------------------------
  expr_ggplot <-
    list(expr(ggplot(aes(!!!aes.args)))) %>%
    c(ifelse(smooth,
             list(expr(stat_smooth(!!!geom.args))),
             list(expr(geom_line())))
    )

  # add styling ggplot functions -----------------------------------------------
  if (type == "net_benefit") {
    y_axis_title <- "Net Benefit"
    ylim = c(x$prevalence * -0.1, x$prevalence) %>% unname()
  }
  else if (type == "net_intervention_avoided") {
    y_axis_title <- paste("Net reduction in interventions\nper",
                          x$net_interventions_nper, "patients")
    ylim = c(0, x$net_interventions_nper)
  }
  else if (type == "standardized_net_benefit") {
    y_axis_title <- "Standardized Net Benefit"
    ylim = c(-0.02, 1.02)
  }
  labs.args <-
    list("Threshold Probability", y_axis_title, "") %>%
    rlang::set_names(c("x", "y", ifelse(style == "color", "color", "linetype")))

  expr_ggplot <-
    expr_ggplot %>%
    c(list(
      expr(coord_cartesian(ylim = !!ylim)),
      expr(scale_x_continuous(labels = scales::percent_format(accuracy = 1))),
      expr(labs(!!!labs.args)),
      expr(theme_bw())
    ))

  # construct data prep and ggplot expressions, and return plot ----------------
  str_final_expression <-
    c(
      expr_data_prep %>%
        purrr::map(rlang::quo_text) %>%
        paste(collapse = " %>%\n  "),
      expr_ggplot %>%
        purrr::map(rlang::quo_text) %>%
        paste(collapse = " +\n  ")
    ) %>%
    paste(collapse = " %>%\n  ")

  # show ggplot code if requested
  if (isTRUE(show_ggplot_code)) {
    cat("# ggplot2 code to create DCA figure -------------------------------\n")
    cat(str_final_expression)
  }

  # return ggplot
  str_final_expression %>%
    rlang::parse_expr() %>%
    eval()
}
