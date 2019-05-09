

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





objective <- makeMobafeasObjective(makeLearner("classif.logreg"), pid.task, pSS(), cv10)
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

ctrl <- makeMBFControl(mosmafs.config)

mbo(objective, control = ctrl)
