

#' @title Start mobafeas MBO
#'
#' @param fun `[smoof_function]` smoof function with additional attributes `"co.objective"` and `"nadir"`.
#'   `"co.objective"` is a `function` taking the design `data.frame` and returning a vector of second-objective-values.
#'   `"nadir"` is a `numeric(2)` nadir-point for objective 1 (of the `fun` return value) and 2 (the `"co.objective"`
#'   return value).
#' @param population `[list]` list of parameter values for initial design.
#' @export
mobafeasMBO <- function(fun, population, learner = NULL, control, show.info = getOption("mlrMBO.show.info", TRUE), more.args = list()) {
  if (!all.equal(attr(fun, "n.objectives"), 1)) {
    stop("fun must be a 2-objective SMOOF function.")
  }
  nullfeat <- population[[1]]
  nullfeat$selector.selection <- 0L * nullfeat$selector.selection
  population <- c(population, list(nullfeat))
  ps <- getParamSet(fun)

  control$co.objective <- attr(fun, "co.objective")
  assertFunction(control$co.objective, nargs = 1)
  control$nadir <- attr(fun, "nadir")
  assertNumeric(control$nadir, any.missing = FALSE, len = 2)

  mbo(fun = fun, design = mosmafs:::listToDf(population, ps), learner = learner, control = control, show.info = show.info, more.args = more.args)
  ## ymatrix <- sapply(population, function(v) {
  ##   trafoValue(ps, v) %>% valuesFromNames(paramset = ps) %>% fun
  ## })
  ## rownames(ymatrix) <- ctrl$y.name
  ## design <- cbind(mosmafs:::listToDf(population, ps), t(ymatrix))
  ## initSMBO(par.set = getParamSet(fun), design = design, learner = learner, control = control, show.info = show.info)
}
