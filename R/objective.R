


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
#' @param multi.objective `[logical(1) | function]` Whether to create a multi-objective
#'   function. Default `TRUE`. `FALSE`: simulate single-objective optimization by
#'   setting the second-objective value to 0 by default. This may also be a function with
#'   two inputs `performance`, `featfrac` that computes a scalarized objective. The scalarized
#'   objective must always be minimized.
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
  assert(
      checkFlag(multi.objective),
      checkFunction(multi.objective)
  )
  if (is.null(worst.measure)) {
    worst.measure <- measure$worst
  }
  assertNumber(worst.measure)
  if (is.function(multi.objective)) {
    trafo.fun <- multi.objective
    multi.objective <- FALSE
  } else {
    obj.factor <- if (measure$minimize) 1 else -1
    trafo.fun <- function(performance, featfrac) {
      performance * obj.factor
    }
  }
  worst.measure <- trafo.fun(worst.measure, 1)
  ps <- c(ps, pSS(selector.selection:integer[0, 1]^getTaskNFeats(task)))
  learner <- cpoSelector() %>>% checkLearner(learner, type = getTaskType(task))
  learner %<<<% cpo
  argnames <- getParamIds(getParamSet(learner))
  fun <- smoof::makeSingleObjectiveFunction(sprintf("mosmafs_%s_%s", learner$id, task$task.desc$id),
    has.simple.signature = FALSE, par.set = ps, noisy = TRUE,
    fn = function(x) {
      args <- x
      propfeat <- mean(args$selector.selection)
      args <- args[intersect(names(args), argnames)]
      args$selector.selection <- as.logical(args$selector.selection)
      learner <- setHyperPars(learner, par.vals = args)
      untransformed.val <- resample(learner, task, resampling, list(measure), show.info = FALSE)$aggr

      if (is.na(val)) {
        ret <- worst.measure
      } else {
        ret <- unname(trafo.fun(untransformed.val, propfeat))
      }
      # c(perf = ret, propfeat = propfeat)
      extras <- list(untransformed.val = untransformed.val, propfeat = propfeat)
      if (!is.null(holdout.data)) {
        model <- train(learner, task)
        prd <- predict(model, holdout.data)
        untransformed.holdout <- performance(prd, list(measure), task, model)[1]
        if (is.na(val)) {
          holdout <- worst.measure
        } else {
          holdout <- unname(trafo.fun(untransformed.holdout, propfeat))
        }
        extras$holdout <- holdout
        extras$untransformed.holdout <- untransformed.holdout
      }
      attr(ret, "etras") <- extras
      ret
    })
  attr(fun, "co.objective") <- if (multi.objective) function(design) {
    apply(design[grepl("^selector\\.selection[0-9]+$", colnames(design))], 1, mean)
  } else function(design) {
    rep(0, nrow(design))
  }
  attr(fun, "nadir") <- c(worst.measure, 1)
  fun
}
