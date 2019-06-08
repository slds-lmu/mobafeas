# SETUP: clone BOCS into main directory from https://github.com/juliambr/BOCS
# python 2.7 needs to be installed as well as 
# dependencies cvxpy, cvxopt (python)

# start form R

library("mlr") 
library("mlrMBO")
library("reticulate")
library("smoof")
library("data.table")

task = sonar.task
lrn =  makeLearner("classif.ksvm", kernel = "rbfdot")
stratcv = makeResampleDesc("CV", iters = 2L, stratify = TRUE)

setwd("BOCS/BOCSpy")
# load the python package
source_python("../../runBOCS.py")

# save lrn, task, resampling
# we will call the python function BOCS
# BOCS calls an R script for function evaluation 
saveRDS(lrn, "lrn.rds")
saveRDS(task, "tasktrain.rds")
saveRDS(stratcv, "stratcv.rds")

p = getTaskNFeats(task)

# running BOCS with simmulated annealing works
z = runBOCS(n_vars = p, n_init = 4L, eval_budget = 6L, sim_anneal = TRUE, lamb = 10^(-4))

# running BOCS with SQP doesn't work
z = runBOCS(n_vars = p, n_init = 4L, eval_budget = 6L, sim_anneal = FALSE, lamb = 10^(-4))
