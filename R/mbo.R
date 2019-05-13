
#' @export
mobafeasMBO <- function(fun, population, learner = NULL, control, show.info = getOption("mlrMBO.show.info", TRUE), more.args = list()) {
  if (!all.equal(attr(fun, "n.objectives"), 1)) {
    stop("fun must be a 2-objective SMOOF function.")
  }
  nullfeat <- population[[1]]
  nullfeat$selector.selection <- 0L * nullfeat$selector.selection
  population <- c(population, list(nullfeat))
  ps <- getParamSet(fun)


  mbo(fun = fun, design = mosmafs:::listToDf(population, ps), learner = learner, control = control, show.info = show.info, more.args = more.args)
  ## ymatrix <- sapply(population, function(v) {
  ##   trafoValue(ps, v) %>% valuesFromNames(paramset = ps) %>% fun
  ## })
  ## rownames(ymatrix) <- ctrl$y.name
  ## design <- cbind(mosmafs:::listToDf(population, ps), t(ymatrix))
  ## initSMBO(par.set = getParamSet(fun), design = design, learner = learner, control = control, show.info = show.info)
}
