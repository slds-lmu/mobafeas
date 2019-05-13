

#' @export
infillOptMobafeas <- function(infill.crit, models, control, par.set, opt.path, designs, iter, ...) {

  infill.crit <- attr(infill.crit, "infill.crit.object")

  # --- TODO: this is the same as in as.MBOInfillCrit.Infill in InfillCrits.R
  model <- models[[1]]
  design <- designs[[1]]
  design.X <- design
  design.X[[control$y.name]] <- NULL
  pointdata <- predict(object = model, newdata = design.X)$data[c("response", "se")]
  pointdata$value <- design[[control$y.name]]
  pointdata$c2 <- apply(design.X[grepl("^selector\\.selection[0-9]+$", colnames(design.X))], 1, mean)
  popmatrix <- rbind(pointdata$value, pointdata$c2)
  paretofront <- popmatrix[, ecr::nondominated(popmatrix), drop = FALSE]
  paretofront <- paretofront[, order(paretofront[2, ]), drop = FALSE]
  nugget <- model$nugget # TODO
  # --- TODO end

  par.set$pars <- lapply(par.set$pars, function(x) {
    x$trafo <- NULL
    x
  })

  nadir <- c(Inf, 1)



  ecrcall <- control$mosmafs

  ecrcall$population <- initSelector(sampleValues(par.set, ecrcall$mu, discrete.names = TRUE))


  ecrcall$mu <- NULL
  ecrcall$fitness.fun <- smoof::makeSingleObjectiveFunction("infillopt", has.simple.signature = FALSE, par.set = par.set,
    noisy = TRUE, fn = function(args) {
      newdesign <- convertDataFrameCols(args, ints.as.num = TRUE, logicals.as.factor = TRUE)
      suggestions <- predict(object = model, newdata = newdesign)$data[c("response", "se")]
      suggestions$c2.value <- apply(newdesign[grepl("^selector\\.selection[0-9]+$", colnames(newdesign))], 1, mean)
      -acquisition(infill.crit, suggestions, nadir, paretofront, pointdata, nugget)
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

