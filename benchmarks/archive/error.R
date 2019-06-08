library("mlr") 
library("mlrMBO")
library("reticulate")
library("smoof")
library("data.table")

task = sonar.task

lrn =  makeLearner("classif.ksvm", kernel = "rbfdot")
stratcv = makeResampleDesc("CV", iters = 2L, stratify = TRUE)

# load the package
setwd("BOCS/BOCSpy")
source_python("../../runBOCS.py")

saveRDS(lrn, "lrn.rds")
saveRDS(task, "tasktrain.rds")
saveRDS(stratcv, "stratcv.rds")

p = getTaskNFeats(task)

# running BOCS with simmulated annealing works
z = runBOCS(n_vars = p, n_init = 4L, eval_budget = 6L, sim_anneal = TRUE, lamb = 10^(-4))

# running BOCS with SQP doesn't work
z = runBOCS(n_vars = p, n_init = 4L, eval_budget = 6L, sim_anneal = FALSE, lamb = 10^(-4))
