# MO Bayesian Optimization for Feature Selection


library("mlr")
library("mlrCPO")
library("mlrMBO")
library("mosmafs")

roxygen2::roxygenise("..")

devtools::load_all("..")

x <- function() 1
y <- function() 2

mf <- function(name)  match.fun(name)





InfillResponse()

InfillCB()


acquisition(InfillResponse(), data.frame(response = c(0.5, 0.6, 0.6, 0), se = c(0, 0, 0, 0), c2.value = c(0.5, 0.6, 0.1, 0)), c(1, 1), matrix(c(.8, .2, .5, .5), nrow = 2))

acquisition(InfillCB(), data.frame(response = c(0.5, 0.6, 0.6, 0), se = c(0, 0, 0, 0), c2.value = c(0.5, 0.6, 0.1, 0)), c(1, 1), matrix(c(.8, .2, .5, .5), nrow = 2))

round(
    acquisition(InfillCB(), data.frame(response = c(0.7, 1, 1.2, .8), se = c(0.1, 0.2, 0.3, 0.4), c2.value = c(0.5, 0.6, 0.1, 0)), c(1, 1), matrix(c(.8, .2, .5, .5), nrow = 2)),
    2)


acquisition(InfillEI(), data.frame(response = c(0.7, 1, 1.2, .8), se = c(0.1, 0.2, 0.3, 0.4), c2.value = c(0.5, 0.6, 0.1, 0)), c(1, 1), matrix(c(.8, .2, .5, .5), nrow = 2))

acquisition(InfillEI(), data.frame(response = c(0.5, 0.6, 0.6, 0), se = c(0, 0, 0, 0), c2.value = c(0.5, 0.6, 0.1, 0)), c(1, 1), matrix(c(.8, .2, .5, .5), nrow = 2))


MosmafsConfig(ecr::mutGauss, ecr::recSBX, 70, 15, 10)





objective <- makeMobafeasObjective(makeLearner("classif.logreg"), pid.task, pSS(), cv10, holdout.data = pid.task)
ps.objective <- getParamSet(objective)

mutator.simple <- combine.operators(ps.objective,
  numeric = ecr::setup(mutGauss, sdev = 0.1),
  integer = ecr::setup(mutGaussInt, sdev = 3),
  selector.selection = mutBitflipCHW)

crossover.simple <- combine.operators(ps.objective,
  numeric = recPCrossover,
  integer = recPCrossover,
  selector.selection = recPCrossover)


mosmafs.config <- MosmafsConfig(mutator.simple, crossover.simple, 10)

ctrl <- makeMBFControl(mosmafs.config) %>%
  setMBOControlTermination(10)

setMBO
pop <- sampleValues(ps.objective, 10)

optstate <- mobafeasMBO(objective, population = pop, control = ctrl)

res <- collectMBFResult(optstate)

library("ggplot2")


res$run <- "gp"
res.naive$run <- "naive"


res

ggplot(rbind(res, res.naive), aes(x = perf, y = propfeat, color = run)) + geom_point()
ggplot(rbind(res, res.naive), aes(x = dob, y = train.time, color = run)) + geom_point()
ggplot(rbind(res, res.naive), aes(x = dob, y = naive.hout.domHV, color = run)) + geom_point()

ggplot(res, aes(x = perf, y = propfeat, color = InfillEI)) + geom_point()



optstate$opt.path$env$extra




proposePoints(optstate)




extras

yval

library("kergp")



kern.hamming <- function(f1, f2, par) {
  K <- exp(-sum(abs((f1 - f2) * par)))
  attr(K, "gradient") <- -abs(f1 - f2) * par * K
  K
}

kern.kern <- covMan(kern.hamming, TRUE, d = 2, parLower = c(0, 0), par = c(1, 1), parNames = paste0("par", 1:2))
checkGrad(kern.kern)

inputNames(kern.kern)


x <- sort(runif(40))
X <- cbind(x = x)
yExp <- kergp::simulate(k1Exp, nsim = 20, X = X)
matplot(X, yExp, type = "l", col = "SpringGreen", ylab = "")

yGauss <- kergp::simulate(k1Gauss, nsim = 20, X = X)
matlines(X, yGauss, col = "orangered")

myCov1 <- covTS(kernel = "k1Exp", inputs = c("v1", "v2", "v3"), dep = c(range = "input"))
coef(myCov1) <- c(range = c(0.3, 0.7, 0.9), sigma2 = c(2, 2, 8))
kergp::coef(myCov1, as = "matrix", type = "range")




kmlrn <- makeLearner("regr.km", covtype = "matern3_2", optim.method = "gen", predict.type = "se", nugget.estim = TRUE, jitter = TRUE)

bhx <- subsetTask(bh.task, features = setdiff(getTaskFeatureNames(bh.task), "chas"))

kmat <- kMatern(d = 2, nu = "3/2")
coefUpper(kmat) <- c(4 * diff(apply(bhx$env$data[c("tax", "rad")], 2, range)), 1)
gplrn <- makeLearner("regr.kernel.gp", special.kernel = kmat, special.kernel.features = c("tax", "rad"), default.kernel = "matern3_2", predict.type = "se")

tr1 <- train(gplrn, bhx)
tr2 <- train(gplrn, bhx)
tr3 <- train(gplrn, bhx)

tr1$learner.model
tr2$learner.model
tr3$learner.model

as.list(gplrn$par.vals$savevarenv)

kmat1 <- kmat
kergp::coef(kmat) <- c(1, 2, 1)
kergp::coef(kmat)
kergp::coef(kmat1)

system.time(gptrn <- train(gplrn, bhx), gcFirst = FALSE)
system.time(kmtrn <- train(kmlrn, bhx), gcFirst = FALSE)

attributes(kmtrn$learner.model)$logLik
gptrn$learner.model$logLik
gptrn$learner.model

kmtrn$learner.model

coef(gptrn$learner.model)

bhx2 <- subsetTask(bhx, 1)

predict(kmtrn, bhx2)


DiceKriging::coef(kmtrn$learner.model)
gptrn$learner.model

getTaskFeatureNames(bh.task)

which.col <- "age"
pmat <- t(sapply(do.call(seq, c(as.list(range(bh.task$env$data[[which.col]])), list(length.out = 100))), function(x) {
  l <- getTaskData(bhx2, target.extra = TRUE)$data
  l[[which.col]] <- x
  unlist(l)
}))
plot(predict(kmtrn$learner.model, newdata = pmat, type = "SK")$mean, ylim = range(getTaskData(bh.task, target.extra = TRUE)$target))
lines(predict(kmtrn$learner.model, newdata = pmat, type = "SK")$mean + predict(kmtrn$learner.model, newdata = pmat, type = "SK")$sd)
lines(predict(kmtrn$learner.model, newdata = pmat, type = "SK")$mean - predict(kmtrn$learner.model, newdata = pmat, type = "SK")$sd)
lines(predict(gptrn$learner.model, newdata = pmat, lightReturn = TRUE, seCompute = FALSE, forceInterp = TRUE)$mean, col = "red")
lines(predict(gptrn$learner.model, newdata = pmat)$mean + predict(gptrn$learner.model, newdata = pmat)$sd, col = "red")
lines(predict(gptrn$learner.model, newdata = pmat)$mean - predict(gptrn$learner.model, newdata = pmat)$sd, col = "red")

predict(gptrn, bhx2)

library("parallelMap")
parallelStartMulticore(10)

logdev <- makeMeasure(id = "logdev", minimize = TRUE, best = -Inf, worst = Inf,
  properties = c("regr", "req.pred", "req.truth"),
  note = "Mean of the -dnorm()",
  fun = function(task, model, pred, feats, extra.args) {
    -mean(dnorm(pred$data$response, pred$data$truth, pred$data$se, TRUE))
  }
)

resample(makeLearner("regr.lm", predict.type = "se"), bh.task, cv10, measures = list(logdev))

library("foreach")
library("doParallel")

cl <- makeCluster(5, type = "FORK")
registerDoParallel(cl)

# doParallel::registerDoParallel(parallel::makeForkCluster(2))

gpres5 <- resample(gplrn, bhx, cv10, measures = list(logdev))




kmres2 <- resample(kmlrn, bhx, cv10, measures = list(logdev))

gpres2
kmres2
gpres3
gpres4
gpres5

median(gpres5$measures.test$logdev)
median(gpres4$measures.test$logdev)
median(gpres3$measures.test$logdev)
points(kmres2$measures.test, pch = "+")

performance(gpres2$pred)
performance(kmres2$pred)



kmtrn$learner.model
gptrn$learner.model


makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE, config = config)

gptrn$learner.model$logLik
attributes(kmtrn$learner.model)$logLik


predict(gptrn$learner.model, newdata = as.data.frame(pmat))$mean

kg <- kGauss(d = 1)
kergp::coef(kg) <- c(1, 2)
kergp::coef(kg)

covMat(kg, matrix(c(1, 2, 3, 4), ncol = 1))


kergp::coef(product.kernel)


rt <- makeRegrTask("test",
  data.frame(x = 1 / ((-3:3) + 0.5), y = (-3:3)^2/100 + sin(-3:3)),
  target = "y")

gpone <- makeLearner("regr.kernel.gp", special.kernel = kMatern(d = 2, nu = "3/2"), special.kernel.features = character(0), default.kernel = "matern3_2", predict.type = "se", multistart = 1, noise = TRUE)


devtools::load_all("..")
debugonce(kergp::gp)
gpmod <- train(gpone, rt)

gpmod <- train(gpone, bhx)
kmmod <- train(kmlrn, bhx)

gpmod$learner.model

kmmod$learner.model

kmmod$learner.model@covariance@sd2

kmmod <- train(kmlrn, rt)

X <- seq(min(rt$env$data$x), max(rt$env$data$x), length.out = 234)
prg <- predict(gpmod, newdata = data.frame(x = X))
plot(X, prg$data$response, type = "l", ylim = c(-1, 1))
points(rt$env$data$x, rt$env$data$y)
lines(X, prg$data$response + prg$data$se, type = "l")
lines(X, prg$data$response - prg$data$se, type = "l")
prk <- predict(kmmod, newdata = data.frame(x = X))
lines(X, prk$data$response, type = "l", col = "red")
lines(X, prk$data$response + prk$data$se, type = "l", col = "red")
lines(X, prk$data$response - prk$data$se, type = "l", col = "red")

gpmod$learner.model$logLik
kmmod$learner.model@logLik

gpmod$learner.model
kmmod$learner.model



plot(prk$data$response - prg$data$response)
plot(prk$data$se - prg$data$se)


predict(gpmod$learner.model, newdata = data.frame(x = 0), forceInterp = TRUE)
predict(gpmod$learner.model, newdata = data.frame(x = 0), forceInterp = FALSE)
predict(gpmod$learner.model, newdata = data.frame(x = 0), forceInterp = TRUE)$sd^2 - predict(gpmod$learner.model, newdata = data.frame(x = 0), forceInterp = FALSE)$sd^2
gpmod$learner.model$varNoise


prgfull <- predict(gpmod$learner.model, newdata = data.frame(x = X + 0*rnorm(length(X), 0, 1e-7)), forceInterp = TRUE)

names(prgfull)

prkfull <- predict(kmmod$learner.model, newdata = data.frame(x = X + rnorm(length(X), 0, 1e-7)), type = "SK")

names(prkfull)

plot(prgfull$sd + gpmod$learner.model$varNoise - prkfull$sd)

str(prgfull)
str(prkfull)


# optim method 'gen' in dicekriging:
# - pop.size min(20, floor(4 + 3 * log( [ncol(design)] )))
# - max.generations 5
# - wait.generations 2
# - BFGSburnin 0
# - trace 1
#
# rgenoud:
# - fn: fn ???
# - nvars: length(parinit)
# - max, starting.values, Domains: cbind(lower, upper)
# - gr: gr
# - gradient.check FALSE
# - boundary.enforcement 2
# - hessian TRUE
# - optim.method L-BFGS-B
# - model: model
# - envir: envir

storedLogLikGrad <- function(par) {
  our.par <- gradEnv$par
  if (!all.equal(par, our.par)) stop("bad par")
  gradEnv$LLgrad
}



opt <- try(rgenoud::genoud(
  fn = negLogLikFun,
  nvars = length(parNames),
  max = FALSE,
  pop.size = min(20, floor(5 + 3 * log(NVARS))),
  max.generations = 6,
  wait.generations = 3,
  hard.generation.limit = TRUE,
  starting.values = parIni,
  Domains = cbind(parLower, parUpper),
  gr = thisLogLikGrad,
  boundary.enforcement = 2,
  gradient.check = FALSE,
  optim.method = "L-BFGS-B",
  trace = 1,
  BFGSburnin = 0), silent = TRUE)



# ---------------------------------------------------------------

objective <- makeMobafeasObjective(makeLearner("classif.knn"), pid.task, pSS(k: integer[1, 30]), cv10, holdout.data = pid.task)
objective.singleobj <- makeMobafeasObjective(makeLearner("classif.knn"), pid.task, pSS(k: integer[1, 30]), cv10, holdout.data = pid.task, multi.objective = FALSE)
objective.scalarized <- makeMobafeasObjective(makeLearner("classif.knn"), pid.task, pSS(k: integer[1, 30]), cv10, holdout.data = pid.task,
  multi.objective = function(perf, featfrac) perf + featfrac)

ps.objective <- getParamSet(objective)

mutator.simple <- combine.operators(ps.objective,
  numeric = ecr::setup(mutGauss, sdev = 0.1),
  integer = ecr::setup(mutGaussInt, sdev = 3),
  selector.selection = mutBitflipCHW)

crossover.simple <- combine.operators(ps.objective,
  numeric = recPCrossover,
  integer = recPCrossover,
  selector.selection = recPCrossover)

mosmafs.config <- MosmafsConfig(mutator.simple, crossover.simple, 10)

ctrl <- makeMBFControl(mosmafs.config) %>%
  setMBOControlTermination(100)

pop <- sampleValues(ps.objective, 10)

optstate.naive <- mobafeasMBO(objective, population = pop, control = ctrl)

optstate.our.naive <- mobafeasMBO(objective, population = pop, control = ctrl,
  learner = constructMBFLearner(ps.objective))

optstate.rf <- mobafeasMBO(objective, population = pop, control = ctrl,
  learner = constructRFSurrogate())

optstate <- mobafeasMBO(objective, population = pop, control = ctrl,
  learner = constructMBFLearner(ps.objective, kernelMBFHamming))

optstate.singleobj <- mobafeasMBO(objective.singleobj, population = pop, control = ctrl,
  learner = constructMBFLearner(ps.objective, kernelMBFHamming))

optstate.scalarized <- mobafeasMBO(objective.scalarized, population = pop, control = ctrl,
  learner = constructMBFLearner(ps.objective, kernelMBFHamming))


res <- collectMBFResult(optstate)
res.naive <- collectMBFResult(optstate.naive)
res.our.naive <- collectMBFResult(optstate.our.naive)
res.rf <- collectMBFResult(optstate.rf)
res.singleobj <- collectMBFResult(optstate.singleobj)
res.scalarized <- collectMBFResult(optstate.scalarized)



res$run <- "res"
res.naive$run <- "naive"
res.our.naive$run <- "our.naive"
res.rf$run <- "rf"
res.singleobj$run <- "singleobj"
res.scalarized$run <- "scalarized"


library("ggplot2")
data <- rbind(res, res.naive, res.our.naive, res.rf, res.singleobj, res.scalarized)

ggplot(data, aes(x = perf, y = propfeat, color = run)) + geom_point()

ggplot(data, aes(x = dob, y = untransformed.val, color = run)) + geom_point()
ggplot(data, aes(x = dob, y = propfeat, color = run)) + geom_point()
ggplot(data, aes(x = perf, y = untransformed.val, color = run)) + geom_point()
ggplot(data, aes(x = perf, y = untransformed.val + propfeat, color = run)) + geom_point()


ggplot(data, aes(x = untransformed.val, y = propfeat, color = run)) + geom_point()

res.singleobj



ggplot(data, aes(x = dob, y = train.time, color = run)) + geom_point()
ggplot(data, aes(x = dob, y = propose.time, color = run)) + geom_point()
ggplot(data, aes(x = dob, y = naive.hout.domHV, color = run)) + geom_point()
ggplot(data, aes(x = dob, y = true.hout.domHV, color = run)) + geom_point()
ggplot(data, aes(x = dob, y = true.hout.domHV, color = run)) + geom_point()
ggplot(data, aes(x = dob, y = propose.time, color = run)) + geom_point()
head(data)

checkGrad(kernelMBFHamming(3))


# --------------------------------------------------------------

     myCovMan <-
           covMan(
              kernel = function(x1, x2, par) {
              htilde <- (x1 - x2) / par[1]
              SS2 <- sum(htilde^2)
              d2 <- exp(-SS2)
              kern <- par[2] * d2
              d1 <- 2 * kern * SS2 / par[1]
              attr(kern, "gradient") <- c(theta = d1,  sigma2 = d2)
              return(kern)
           },
           hasGrad = TRUE,
           d = 1,
           label = "myGauss",
           parLower = c(theta = 0.0, sigma2 = 0.0),
           parUpper = c(theta = Inf, sigma2 = Inf),
           parNames = c("theta", "sigma2"),
           par = c(1, 2)
           )

