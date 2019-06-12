library(batchtools)
library(dplyr)
library(mlr)
library(mlrCPO)

source("helpers.R")
source("probdesign.R")

# load registry
reg = loadRegistry("registry", writeable = FALSE)
tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval", "cv.iters", "sim_anneal", "initialization",
	"lambda", "ninit", "objective", "kernel", "joint.hyperpars", "parallelize"))

done = ijoin(tab, findDone())

path = "results_raw"
dir.create(path)

problems = c("wdbc", "ionosphere", "sonar", "hill-valley", "clean1", 
  "tecator", "USPS", "madeline", "lsvt", "madelon", "isolet", "cnae-9")

mbfres = reduceResultsDataTable(done[!is.na(initialization), ], fun = function(x) collectMBFResult(x$result))
mbfres = ijoin(mbfres, tab)

saveRDS(mbfres, file.path(path, "res.rds"))