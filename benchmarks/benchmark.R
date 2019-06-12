# ---
# BENCHMARK MOBAFEAS
# ---


# ---
# 0. Load libraries
# ---

# TODO: packrat

packages = c("mlr", "mlrCPO", "mlrMBO", "mosmafs", "reticulate", "smoof",
             "batchtools", "data.table", "parallelMap", "magrittr", "ParamHelpers",
             "mobafeas")

lapply(packages, library, character.only = TRUE)

# ---
# 1. Setup envorinoment (TEST / NO TEST) + load registry
# ---

TEST = FALSE

if (TEST) {
  deffile = "def_test.R"
  registry_name = "registry_test"
} else {
  deffile = "def.R"
  registry_name = "registry2"
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


# ---
# 2. Create problems
#     - for each dataset we store the path
#     - dataset + resampling instance = problem
# ---


readDataAndRinst = function(data, job, rinst.iter, ...) {
  task = readRDS(file.path(data, "task.rds"))
  rin = readRDS(file.path(data, "rin.rds"))

  train.task = subsetTask(task, rin$train.inds[[rinst.iter]])
  test.task = subsetTask(task, rin$test.inds[[rinst.iter]])
  
  list(train.task = train.task, test.task = test.task)
}

for (i in 1:length(datasets)) {  
  addProblem(
    name = names(datasets)[i], 
    data = paste(datafolder, names(datasets)[i], sep = "/"), 
    fun = readDataAndRinst,
    reg = reg
    )
}

# ---
# 3. Add algorithms
# ---

# Bayesiona Optimization on Combinatorial Structures
source("algorithms/BOCS.R")
addAlgorithm(name = "BOCS", reg = reg, fun = BOCS)

# Mobafeas
source("algorithms/mobafeas.R")
addAlgorithm(name = "mobafeas", reg = reg, fun = mobafeas)


# ---
# 4. Add Experiments
# ---

addExperiments(reg = reg, 
  prob.designs = pdes,
  algo.designs = list(
  BOCS = ades.BOCS, 
  mobafeas = ades.mobafeas), 
  repls = REPLICATIONS)
