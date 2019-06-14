# Testing MBONaive

library(testthat)

source("def.R")
source("probdesign.R")
source("algorithms/MBONaive.R")

# load packages
lapply(packages, library, character.only = TRUE)



data = NULL
job = NULL 
instance = list(train.task = readRDS("data/sonar/task.rds"), test.task = readRDS("data/sonar/task.rds"))
learner = "SVM"
cv.iters = 2L
ninit = 10L
maxeval = 15L
maxtime = 60L
filter.methods = c("anova.test", "auc", "mrmr", "ranger.impurity")

# No feature selection
# Single objective
res = naiveMBO(data, job, instance, learner, cv.iters, ninit, objective = "SO", feature.sel = "none", 
	filter.methods, maxeval, maxtime, parallelize = FALSE)
expect_true(res$result$y < 1)


# Scalar
res = naiveMBO(data, job, instance, learner, cv.iters, ninit, objective = "scalar", feature.sel = "none", 
	filter.methods, maxeval, maxtime, parallelize = FALSE)
expect_true(res$result$y > 1)

# Multi-objective
res = naiveMBO(data, job, instance, learner, cv.iters, ninit, objective = "MO", feature.sel = "none", 
	filter.methods, maxeval, maxtime, parallelize = FALSE)

expect_true(res$result$y > 1)



naiveMBO = function(data, job, instance, learner, cv.iters, ninit, objective, 
  feature.sel, filter.methods, maxeval, maxtime, parallelize)


# Test if filters computed are done correctly



# Test if stuff is really done MO if we want it to 



