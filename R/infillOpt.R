
#' @title MOSMAFS-based Multi-Crit Infill-Optimization
#'
#' @description
#' Uses [`mosmafs::mosmafs-package`] to perform infill optimization. The infill criteria are not
#' the ones from `mlrMBO`, but instead the ones given by an [`Infill`] object.
#'
#' This function should probably not be called directly, but instead should be given to `mlrMBO::mbo`
#' as the infill criterion. This is usually done using `makeMBFControl()` and then `mobafeasMBO()`.
#'
#' @param infill.crit `[Infill]` Infill criterion being used.
#' @param models `[list of WrappedModel]` Model(s) trained on the performance data so far. Only the
#'   first model is considered.
#' @param control `[MBOControl]` `mlrMBO` control object, as augmented by `makeMBFControl()` and
#'   `mobafeasMBO()`.
#' @param par.set `[ParamSet]` decision space over which MBO-optimization is performed.
#' @param opt.path `[OptPath]` OptPath of results so far
#' @param designs `[list of data.frame]` data.frame of results so far
#' @param iter `[integer(1)]` iteration number. Ignored.
#' @return `data.frame` of suggested point to evaluate.
#' @export
infillOptMobafeas <- function(infill.crit, models, control, par.set, opt.path, designs, iter, ...) {

  infill.crit <- attr(infill.crit, "infill.crit.object")

  population.info <- computePopulationInfo(models[[1]], designs[[1]], control)

  par.set$pars <- lapply(par.set$pars, function(x) {
    x$trafo <- NULL
    x
  })

  ecrcall <- control$mosmafs

  ecrcall$population <- initSelector(sampleValues(par.set, ecrcall$mu, discrete.names = TRUE))


  ecrcall$mu <- NULL
  ecrcall$fitness.fun <- smoof::makeSingleObjectiveFunction("infillopt", has.simple.signature = FALSE, par.set = par.set,
    noisy = TRUE, fn = function(args) {
      newdesign <- convertDataFrameCols(args, ints.as.num = TRUE, logicals.as.factor = TRUE)
      suggestions <- predict(object = models[[1]], newdata = newdesign)$data[c("response", "se")]
      suggestions$c2.value <- control$co.objective(newdesign)
      -acquisition(infill.crit, suggestions, population.info)
    }) %>% setMosmafsVectorized()

  opt <- do.call(slickEcr, ecrcall)
  mosmafs:::listToDf(opt$best.x, par.set)
}

#' @title Mosmafs Configuration Object
#'
#' @description
#' Configures the mosmafs infill optimization to be run during model-based optimization in `infillOptMobafeas()`.
#'
#' See `mosmafs::slickEcr` for more details on parameters.
#'
#' @param mutator `[ecr_mutator]` mutator
#' @param recombinator `[ecr_recombinator]` recombinator
#' @param generations `[integer(1) | list of function]` number of generations to run, or termination criterion. Note this
#'   only specifies the termination criterion for the infill optimization that is performed once per target function evaluation.
#' @param mu `[integer(1)]` number of starting individuals.
#' @param lambda `[integer(1)]` number of individuals to add each generation
#' @param parent.selector `[ecr_selector]` parent selection operator.
#' @param survival.selector `[ecr_selector]` survival selection operator.
#' @param survival.strategy `[character(1) | function]` one of `"plus"`, `"comma"` or a function.
#' @param n.elite `[integer(1)]` for `"comma"` survival strategy: number of elites that survive each generation.
#' @param p.mut `[numberic(1)]` individual-wise mutation probability.
#' @param p.recomb `[numeric(1)]` individual-pair-wise recombination probability.
#' @return `MosmafsConfig` object.
#'
#' @family Control Objects
#' @export
MosmafsConfig <- function(mutator, recombinator, generations, mu = 80, lambda = 15, parent.selector = ecr::selTournament, survival.selector = ecr::selGreedy, survival.strategy = "plus", n.elite = 0, p.mut = 0.3, p.recomb = 0.7) {
  assertClass(mutator, "ecr_mutator")
  assertClass(recombinator, "ecr_recombinator")
  assertInt(mu, lower = 1)
  assertInt(lambda, lower = 1)
  assert(checkInt(generations, lower = 0), checkList(generations, type = "function"))
  assertClass(parent.selector, "ecr_selector")
  assertClass(survival.selector, "ecr_selector")
  assert(checkChoice(survival.strategy, c("plus", "comma")), checkFunction(survival.strategy))
  assertInt(n.elite, lower = 0)
  if ("single-objective" %nin% attr(parent.selector, "supported.objectives")) {
    stopf("parent.selector does not support single-objective optimization.")
  }
  if ("single-objective" %nin% attr(survival.selector, "supported.objectives")) {
    stopf("parent.selector does not support single-objective optimization.")
  }
  assertNumber(p.mut, lower = 0, upper = 1)
  assertNumber(p.recomb, lower = 0, upper = 1)
  makeConfigObject("MosmafsConfig")
}

# compute population info needed by most infill criteria.
# @param model `[WrappedModel]` the surrogate model fitted to the design and their performance
# @param design `[data.frame]` design of parameter settings and performance values
# @param control `[MBOControl]` mlrMBO control object
# @return `list` with entries `pointdata`: model predictions for each row in design,
#   `paretofront`: pareto-front matrix with two rows and <pareto front size> columns,
#   `nugget`: surrogate model nugget value, `nadir`: given nadir of multi-objective problem.
computePopulationInfo <- function(model, design, control) {
  design.X <- design
  design.X[[control$y.name]] <- NULL
  pointdata <- predict(object = model, newdata = design.X)$data[c("response", "se")]
  pointdata$value <- design[[control$y.name]]
  pointdata$c2 <- control$co.objective(design.X)
  popmatrix <- rbind(pointdata$value, pointdata$c2)
  paretofront <- popmatrix[, ecr::nondominated(popmatrix), drop = FALSE]
  paretofront <- paretofront[, order(paretofront[2, ]), drop = FALSE]
  nugget <- getNugget(model$learner.model)

  list(pointdata = pointdata, paretofront = paretofront, nugget = nugget, nadir = control$nadir)
}

#' @title Get Nugget from Model
#'
#' @description
#' Return the nugget value of the (Gaussian process) model.
#'
#' Currently defined for `DiceKriging` and `kergp` models.
#'
#' @param learner.model `[model object]` the model to query.
#' @return `numeric(1)` nugget value.
#'
#' @family Utility Functions
getNugget <- function(learner.model) {
  UseMethod("getNugget")
}

#' @export
getNugget.default <- function(learner.model) {
  NULL
}

#' @export
getNugget.DiceKriging <- function(learner.model) {
  learner.model@covariance@nugget
}

#' @export
getNugget.gp <- function(learner.model) {
  learner.model$varNoise
}
