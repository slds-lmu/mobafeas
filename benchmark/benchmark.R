library("mlr") 
library("mlrCPO")
library("mlrMBO")
library("mosmafs")
library("reticulate")
library("smoof")
library("batchtools")
library("data.table")

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


# return the filepath for each 
for (ds in datasets) {  
  addProblem(name = ds, data = paste(datafolder, ds, sep = "/"), reg = reg)
}

source("algorithms/BOCS.R")

source("algorithms/MBONaive.R")

source("algorithms/randomsearch.R")


addAlgorithm(name = "BOCS", reg = reg, fun = BOCS)
addAlgorithm(name = "randomsearch", reg = reg, fun = randomsearch)
addAlgorithm(name = "MBONaive", reg = reg, fun = MBONaive)

addExperiments(reg = reg, 
  algo.designs = list(BOCS = ades.BOCS, 
    randomsearch = ades.randomsearch,
    MBONaive = ades.mboNaive), 
  repls = REPLICATIONS)



