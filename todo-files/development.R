

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

ggplot(res, aes(x = perf, y = propfeat, color = dob)) + geom_point()
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

gplrn <- makeLearner("regr.kernel.gp", special.kernel = kMatern(d = 2, nu = "3/2"), special.kernel.features = c("tax", "rad"), default.kernel = "matern3_2", predict.type = "se", multistart = 5)

system.time(gptrn <- train(gplrn, bhx), gcFirst = FALSE)
system.time(kmtrn <- train(kmlrn, bhx), gcFirst = FALSE)

attributes(kmtrn$learner.model)$logLik

coef(gptrn$learner.model)

bhx2 <- subsetTask(bhx, 1)

predict(kmtrn, bhx2)


DiceKriging::coef(kmtrn$learner.model)
gptrn$learner.model

getTaskFeatureNames(bh.task)

which.col <- "nox"
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
  note = "Sum of the -dnorm()",
  fun = function(task, model, pred, feats, extra.args) {
    -sum(dnorm(pred$data$response, pred$data$truth, pred$data$se, TRUE))
  }
)

resample(makeLearner("regr.lm", predict.type = "se"), bh.task, cv10, measures = list(logdev))

library("foreach")
library("doParallel")

cl <- makeCluster(5, type = "FORK")
registerDoParallel(cl)

# doParallel::registerDoParallel(parallel::makeForkCluster(2))

gpres3 <- resample(gplrn, bhx, cv10, measures = list(logdev))
kmres2 <- resample(kmlrn, bhx, cv10, measures = list(logdev))

gpres2
kmres2
gpres3

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
