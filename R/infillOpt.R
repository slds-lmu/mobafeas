

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


getNugget <- function(learner.model) {
  UseMethod("getNugget")
}

getNugget.default <- function(learner.model) {
  NULL
}

getNugget.DiceKriging <- function(learner.model) {
  learner.model@covariance@nugget
}

getNugget.gp <- function(learner.model) {
  learner.model$varNoise
}
