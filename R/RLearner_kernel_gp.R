
#' @export
makeRLearner.regr.kernel.gp <- function() {
  makeRLearnerRegr(
    cl = "regr.kernel.gp",
    package = c("kergp", "rgenoud"),
    par.set = pSSLrn(
      default.kernel: discrete [matern5_2, matern3_2, gauss, exp],
      special.kernel: untyped,
      special.kernel.features: untyped,
      noise = TRUE: logical,
      savevarenv: untyped
    ),
    par.vals = list(default.kernel = "matern5_2", savevarenv = new.env(parent = emptyenv())),
    properties = c("numerics", "se"),
    name = "Kernel GP",
    short.name = "kernel.gp",
    callees = "kergp"
  )
}

#' @export
trainLearner.regr.kernel.gp <- function(.learner, .task, .subset, .weights = NULL, default.kernel, special.kernel, special.kernel.features, savevarenv, ...) {
  featnames <- getTaskFeatureNames(.task)

  assertCharacter(special.kernel.features, any.missing = FALSE, unique = TRUE)
  assertSubset(special.kernel.features, featnames)

  default.features <- setdiff(featnames, special.kernel.features)

  data <- getTaskData(.task, .subset)
  maxvar <- var(data[[getTaskTargetNames(.task)]])
  d <- length(default.features)
  if (d > 0) {
    if (default.kernel == "gauss") {
      def.cov <- kGauss(d = d)
    } else {
      def.cov <- kMatern(d = d, nu = switch(default.kernel,
        matern5_2 = "5/2", matern3_2 = "3/2", exp = "1/2"))
    }
    inputNames(def.cov) <- default.features
    upper.bounds <- 4 * pmax(sapply(data[default.features], function(x) max(x) - min(x)), 1e-3)
    coefUpper(def.cov) <- c(upper.bounds, maxvar)
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

  if (identical(names(savevarenv$savedvars), names(kergp::coef(product.kernel)))) {
    cat("Using saved vars\n")
    kergp::coef(product.kernel) <- pmin(coefUpper(product.kernel), pmax(coefLower(product.kernel), savevarenv$savedvars))
    vni <- min(maxvar, max(0, savevarenv$vni %??% (maxvar / 10)))
  } else {
    cat("Not using saved vars\n")
    vni <- maxvar / 10
  }

  form <- x ~ 1
  form[[2]] <- asQuoted(getTaskTargetNames(.task))
  # TODO: this sucks
#  cl <- parallel::makeCluster(10, type = "FORK")
#  registerDoParallel(cl)
#  on.exit(stopCluster(cl))

  opt.code <- "
  llfun <- function(par) {
    par <- pmin(parUpper, pmax(parLower, par))
    if (parTrack) {
      gradEvn$parTracked <- rbind(gradEnv$parTracked, par)
    }
    .logLikFun0(par, object, y = thisy, X, F = thisF, compGrad = compGrad,
      noise = noise, gradEnv = gradEnv, trace = trace)
  }

  storedLogLikGrad <- function(par) {
    our.par <- gradEnv$par
    reldiff <- max(abs( ifelse(par == our.par, 0, par - our.par) / max(par + our.par, 1e-5) ))
    if (!isTRUE(1e-4 > reldiff)) {
      cat(sprintf('Difference: %.3g\nrequested: (%s)\npresent:   (%s)\n',
          reldiff,
          paste(sprintf('%.3g', par), collapse = ', '),
          paste(sprintf('%.3g', our.par), collapse = ', ')))
      par <- pmin(parUpper, pmax(parLower, par))
      .logLikFun0(par, object, y = thisy, X, F = thisF, compGrad = compGrad,
        noise = noise, gradEnv = gradEnv, trace = trace)
      our.par <- gradEnv$par
      reldiff <- max(abs( ifelse(par == our.par, 0, par - our.par) / max(par + our.par, 1e-5) ))
      if (!isTRUE(1e-4 > reldiff)) {
        stop(sprintf('After Reeval: %.3g\nrequested: (%s)\npresent:   (%s)',
            reldiff,
            paste(sprintf('%.3g', par), collapse = ', '),
            paste(sprintf('%.3g', our.par), collapse = ', ')))
      }
    }
    gradEnv$LLgrad
  }

  opt <- rgenoud::genoud(
    fn = llfun,
    nvars = length(parNames),
    max = TRUE,
    pop.size = min(20, floor(5 + 3 * log(length(parNames)))),
    max.generations = 7,
    wait.generations = 2,
    hard.generation.limit = TRUE,
    starting.values = parIni,
    Domains = cbind(parLower, parUpper),
    gr = storedLogLikGrad,
    boundary.enforcement = 2,
    gradient.check = FALSE,
    optim.method = 'L-BFGS-B',
    BFGSburnin = 3,
    print.level = 1,
    control = list(factr = 1e12))
  opt$convergence <- FALSE
  "
# cluster = Ldots$cluster)
  res <- kergp::gp(form, data = data, cov = product.kernel, optimCode = opt.code, varNoiseIni = vni, ...)
  savevarenv$vni <- res$varNoise
  savevarenv$savedvars <- kergp::coef(gptrn$learner.model$covariance)
  res
}

#' @export
predictLearner.regr.kernel.gp <- function(.learner, .model, .newdata, ...) {

  se <- .learner$predict.type != "response"
  p <- predict(.model$learner.model, newdata = .newdata, type = "SK", seCompute = se, lightReturn = TRUE, forceInterp = FALSE)
  if (se)
    cbind(p$mean, sqrt(p$sd^2 + .model$learner.model$varNoise))
  else
    c(p$mean)
}
