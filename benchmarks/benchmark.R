library("mlr") 
library("mlrCPO")
library("mlrMBO")
library("mosmafs")
library("reticulate")
library("smoof")
library("batchtools")
library("data.table")
library("parallelMap")
library("magrittr")
library("ParamHelpers")
library("mobafeas")

# --- create test environment ?
TEST = TRUE

if (TEST) {
  deffile = "def_test.R"
  registry_name = "registry_test"
} else {
  deffile = "def.R"
  registry_name = "registry"
}

source(deffile)

if (TEST) {
    unlink(registry_name, recursive = TRUE)
    reg = makeExperimentRegistry(file.dir = registry_name, seed = 123L,
      packages = packages, source = deffile)
} else {
  if (file.exists(registry_name)) {
    if (OVERWRITE) {
      unlink(registry_name, recursive = TRUE)
      reg = makeExperimentRegistry(file.dir = registry_name, seed = 123L,
        packages = packages, source = deffile)
    } else {
      reg = loadRegistry(registry_name, writeable = TRUE)
    }
  } else {
      reg = makeExperimentRegistry(file.dir = registry_name, seed = 123L,
        packages = packages, source = deffile)
    }
}

readDataAndRinst = function(data, job, rinst.iter, ...) {
  task = readRDS(file.path(data, "task.rds"))
  rin = readRDS(file.path(data, "rin.rds"))

  train.task = subsetTask(task, rin$train.inds[[rinst.iter]])
  test.task = subsetTask(task, rin$test.inds[[rinst.iter]])
  
  list(train.task = train.task, test.task = test.task)
}

# return the filepath for each 
for (i in 1:length(datasets)) {  
  addProblem(
    name = names(datasets)[i], 
    data = paste(datafolder, names(datasets)[i], sep = "/"), 
    fun = readDataAndRinst,
    reg = reg
    )
}
# source("../algorithms/randomsearch.R")

# addAlgorithm(name = "randomsearch", reg = reg, fun = randomsearch)

source("algorithms/BOCS.R")
addAlgorithm(name = "BOCS", reg = reg, fun = BOCS)

source("algorithms/mobafeas.R")
addAlgorithm(name = "mobafeas", reg = reg, fun = mobafeas)

addExperiments(reg = reg, 
  prob.designs = pdes,
  algo.designs = list(BOCS = ades.BOCS, 
    #randomsearch = ades.randomsearch),
    mobafeas = ades.mobafeas), 
  repls = REPLICATIONS)



