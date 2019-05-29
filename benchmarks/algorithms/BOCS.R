# TODO: RESULT OBJECT SHOULD BE APPROPRIATE

BOCS = function(data, job, instance, learner, maxeval, cv.iters, sim_anneal, lambda, ninit) {

  # --- task and learner ---
  train.task = instance$train.task
  test.task = instance$test.task # for outer evaluation
  
  # --- learner and paramset
  lrn = LEARNERS[[learner]]
  ps = PAR.SETS[[learner]]

  # --- inner resampling
  inner = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)

  # --- 1. choose hyperparameters independently 
  # --- on full feature set

  # configure random search tuning 
  tune.iters = maxeval
  ctrl = makeTuneControlRandom(maxit = tune.iters)
  
  lrn.wrp = makeTuneWrapper(lrn, resampling = inner, par.set = ps, control = ctrl, show.info = FALSE)

  # tune the tuning
  time = proc.time()

  parallelMap::parallelStartMulticore(cpus = 8L) 
  # parallelMap::parallelStartSocket(cpus = 8L) 
  
  mod = train(lrn.wrp, train.task)

  # getBMRTuneResults(r$bmr)
  parallelMap::parallelStop()

  tuneres = getTuneResult(mod)
  lrn.tuned = setHyperPars(lrn, par.vals = tuneres$x)
  
  timetune = proc.time() - time 

  # --- we do not tune over parameters ---
  setwd("BOCS/BOCSpy")
  source_python("../../runBOCS.py")

  saveRDS(lrn.tuned, "lrn.rds")
  saveRDS(train.task, "tasktrain.rds")
  saveRDS(inner, "stratcv.rds")

  p = getTaskNFeats(train.task)
  ninit = 10L

  time = proc.time()

  # TODO: determine size of init design (!) 
  z = runBOCS(n_vars = p, n_init = ninit, eval_budget = maxeval, sim_anneal = sim_anneal, lamb = lambda)

  runtime = proc.time() - time

  # --- workaround to get everything in the right "shape"
  design = as.data.frame(rbind(z[[1]]$x_vals, z[[2]]))
  names(design) = paste("x", 1:ncol(design), sep = "")
  y = c(z[[1]]$y_vals, z[[3]])
  design$y = y
  
  result = makeMBOResult(design, ninit = ninit, p)

  return(list(result = result, task.test = task.test, task.train = task.train, runtime = runtime))
} 

