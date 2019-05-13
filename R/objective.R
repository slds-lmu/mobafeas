


#' @title Create mlrMBO Objective Function
#'
#' @description
#' Create an objective function that resamples `learner` on `task`
#' with `resampling` and measures `measure` (optional), together
#' with the number of features selected.
#'
#' The `ParamSet` given should only apply to the learner (and the
#' `cpo` if given); a parameter `selector.selection` is added to it.
#' The complete parameter set of the function can be obtained using
#' `getParamSet()`.
#'
#' `learner` must *not* include a `cpoSelector()` applied to it, this
#' happens automatically within `makeObjective`.
#'
#' @param learner `[Learner]` A [`Learner`][mlr::makeLearner] object to optimize.
#' @param task `[Task]` The [`mlr::Task`] object to optimize on.
#' @param ps `[ParamSet]` The [`ParamSet`][ParamHelpers::makeParamSet] to optimize over.
#' @param resampling `[ResampleDesc | ResampleInst]` The [`ResampleDesc`][mlr::makeResampleDesc] or
#'   [`ResampleInst`][mlr::makeResampleInstance] object to use.
#' @param measure `[Measure | NULL]` The [`Measure`][mlr::makeMeasure] to optimize for.
#'   The default is `NULL`, which uses the `task`'s default `Measure`.
#' @param holdout.data `[Task]` Additional data on which to predict each
#'   configuration after training on `task`.
#' @param worst.measure `[numeric(1)]` worst value for measure to consider,
#'   for dominated hypervolume calculation. Will be extracted from the
#'   given measure if not given, but will raise an error if the extracted
#'   (or given) value is infinite.
#' @param cpo `[CPO]` CPO pipeline to apply before feature selection.
#'   (A CPO that should be applied *after* feature selection should already be
#'   part of `learner` when given). Care should be taken that the
#'   `selector.selection` parameter in `ps` has the appropriate length of
#'   the data that `cpo` emits.
#' @param multi.objective `[logical(1)]` Whether to create a multi-objective
#'   function. Default `TRUE`. `FALSE` not yet supported.
#' @return `function` an objective function for [`mlrMBO::mlr`].
#' @export
makeMobafeasObjective <- function(learner, task, ps, resampling, measure = NULL, holdout.data = NULL, worst.measure = NULL, cpo = NULLCPO, multi.objective = TRUE) {

  if (is.null(measure)) {
    measure <- getDefaultMeasure(task)
  }
  assertClass(learner, "Learner")
  assertClass(cpo, "CPO")
  assertClass(task, "Task")
  assertClass(holdout.data, "Task", null.ok = TRUE)
  assertClass(ps, "ParamSet")
  assert(checkClass(resampling, "ResampleInstance"), checkClass(resampling, "ResampleDesc"))
  assertClass(measure, "Measure")
  assertFlag(multi.objective)
  if (!multi.objective) stop("single objective mobafeas not supported yet")
  if (is.null(worst.measure)) {
    worst.measure <- measure$worst
  }
  assertNumber(worst.measure, finite = TRUE)
  obj.factor <- if (measure$minimize) 1 else -1
  worst.measure <- worst.measure * obj.factor
  ps <- c(ps, pSS(selector.selection:integer[0, 1]^getTaskNFeats(task)))
  learner <- cpoSelector() %>>% checkLearner(learner, type = getTaskType(task))
  learner %<<<% cpo
  argnames <- getParamIds(getParamSet(learner))
  smoof::makeSingleObjectiveFunction(sprintf("mosmafs_%s_%s", learner$id, task$task.desc$id),
    has.simple.signature = FALSE, par.set = ps, noisy = TRUE,
    fn = function(x) {
      args <- x

      args <- args[intersect(names(args), argnames)]
      args$selector.selection <- as.logical(args$selector.selection)
      learner <- setHyperPars(learner, par.vals = args)
      val <- resample(learner, task, resampling, list(measure), show.info = FALSE)$aggr

      if (is.na(val)) {
        val <- worst.measure
      }
      propfeat <- mean(args$selector.selection)
      # c(perf = unname(val * obj.factor), propfeat = propfeat)
      ret <- unname(val * obj.factor)
      if (!is.null(holdout.data)) {
        model <- train(learner, task)
        prd <- predict(model, holdout.data)
        val <- performance(prd, list(measure), task, model)[1]
        if (is.na(val)) {
          val <- worst.measure
        }
        attr(ret, "extras") = list(holdout = unname(val * obj.factor))
      }
      ret
    })
}
