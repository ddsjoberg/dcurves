# dcurves (development version)

* Added a note anytime a user creates a Net Interventions Avoided DCA that includes threshold probabilities above the prevalence.

# dcurves 0.3.0

* Added function `test_consequences()` to calculate the diagnostic accuracy of a risk at several thresholds.

* The returned tibble from `as_tibble.dca()` updated the column name from `"prevalence"` to `"pos_rate"`.

* Users are now able to pass zero and one in `dca(thresholds=)`

* Added CRAN badge to README.

* Adding more consistency checks for the arguments in the `dca()` function.

* Added line break in net intervention avoided y-axis label. (#6)

# dcurves 0.2.0

* Changed name of package from dca to dcurves.

* Major changes to API of all function in the dcurves package.

* Added functions `plot.dca()`, `as_tibble.dca()`, `net_intervention_avoided()`, and `standardized_net_benefit()`.

* Added vignettes.

* Allowing 0 to be included in threshold probabilities. (#3 by @ck37)

# dca 0.1.0

* First release of the R package to implement Decision Curve Analysis.
