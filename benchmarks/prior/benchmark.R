library("mlr") 
library("smoof")
library("batchtools")
library("data.table")
library("mlrCPO")
library("OpenML")

source("def.R")

if (file.exists("registry")) {
  if (OVERWRITE) {
    unlink("registry", recursive = TRUE)
    reg = makeExperimentRegistry(file.dir = "registry", seed = 123L,
      packages = packages, source = "def.R")
  } else {
    reg = loadRegistry("registry", writeable = TRUE)
  }
} else {
    reg = makeExperimentRegistry(file.dir = "registry", seed = 123L,
      packages = packages, source = "def.R")
  }


# return the filepath for each 
for (i in 1:length(datasets)) {  
  addProblem(name = names(datasets)[i], data = datasets[[i]], reg = reg)
}

randomsearch = function(data, job, instance, learner, maxeval, cv.iters) {

  id = data

  # --- task and learner ---
  task = OpenML::getOMLTask(id)

  lrn = LEARNERS[[learner]]

  ps = PAR.SETS[[learner]]

  iters = maxeval * sum(getParamLengths(ps))

  ctrl = makeTuneControlRandom(maxit = iters)

  inner = makeResampleDesc("CV", iters = 2L, stratify = TRUE)
  lrn = makeTuneWrapper(lrn, resampling = inner, par.set = ps, control = ctrl, show.info = FALSE)

  # Outer resampling loop
  
  time = proc.time()

  r = OpenML::runTaskMlr(task, lrn, measures = mmce, verbosity = TRUE, seed = 1, models = TRUE)
  # getBMRTuneResults(r$bmr)

  runtime = proc.time() - time
  
  return(list(result = r, runtime = runtime))
} 

addAlgorithm(name = "randomsearch", reg = reg, fun = randomsearch)

addExperiments(reg = reg, 
  algo.designs = list(randomsearch = ades.random), 
  repls = REPLICATIONS)



