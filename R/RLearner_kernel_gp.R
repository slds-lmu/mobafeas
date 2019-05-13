
#' @export
makeRLearner.regr.kernel.gp <- function() {
  makeRLearnerRegr(
    cl = "regr.kernel.gp",
    package = "kergp",
    par.set = pSSLrn(
      default.kernel: discrete [matern5_2, matern3_2, gauss, exp],
      special.kernel: untyped,
      special.kernel.features: untyped,
      noise = TRUE: logical,
      multistart = 1: integer[1, ]
    ),
    par.vals = list(default.kernel = "matern5_2"),
    properties = c("numerics", "se"),
    name = "Kernel GP",
    short.name = "kernel.gp",
    callees = "kergp"
  )
}

#' @export
trainLearner.regr.kernel.gp <- function(.learner, .task, .subset, .weights = NULL, default.kernel, special.kernel, special.kernel.features, ...) {
  featnames <- getTaskFeatureNames(.task)

  assertCharacter(special.kernel.features, any.missing = FALSE, unique = TRUE)
  assertSubset(special.kernel.features, featnames)

  default.features <- setdiff(featnames, special.kernel.features)

  d = length(default.features)
  if (d > 0) {
    if (default.kernel == "gauss") {
      def.cov <- kGauss(d = d)
    } else {
      def.cov <- kMatern(d = d, nu = switch(default.kernel,
        matern5_2 = "5/2", matern3_2 = "3/2", exp = "1/2"))
    }
    inputNames(def.cov) <- default.features
  }
  if (length(special.kernel.features) > 0) {
    inputNames(special.kernel) <- special.kernel.features
    if (d > 0) {
      kenv <- new.env()
      kenv$def.cov <- def.cov
      kenv$special.kernel <- special.kernel
      product.kernel <- covComp(formula = ~ def.cov() * special.kernel(), where = kenv)
    } else {
      product.kernel <- special.kernel
    }
  } else {
    product.kernel <- def.cov
  }

  form <- x ~ 1
  form[[2]] <- asQuoted(getTaskTargetNames(.task))
  # TODO: this sucks
  cl <- makeCluster(5, type = "PSOCK", port = 11000 + round(runif(1) * 10000))
  registerDoParallel(cl)
  res <- kergp::gp(form, data = getTaskData(.task, .subset), cov = product.kernel, ...)
  stopImplicitCluster()
  res
}

#' @export
predictLearner.regr.kernel.gp <- function(.learner, .model, .newdata, ...) {

  se <- .learner$predict.type != "response"
  p <- predict(.model$learner.model, newdata = .newdata, type = "SK", seCompute = se, lightReturn = TRUE, forceInterp = FALSE)
  if (se)
    cbind(p$mean, p$sd)
  else
    c(p$mean)
}
