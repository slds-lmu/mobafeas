# TODO: RESULT OBJECT SHOULD BE APPROPRIATE

BOCS = function(data, job, instance, learner, 
  maxeval, cv.iters, sim_anneal, lambda, ninit,
  objective) {

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
  
  lrn.wrp = makeTuneWrapper(lrn, resampling = inner, 
    par.set = ps, control = ctrl, show.info = FALSE)

  # tune the tuning
  time = proc.time()

  # parallelMap::parallelStartMulticore(cpus = 8L) 
  # parallelMap::parallelStartSocket(cpus = 8L) 
  
  mod = train(lrn.wrp, train.task)

  # getBMRTuneResults(r$bmr)
  # parallelMap::parallelStop()

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

  time = proc.time()

  # TODO: determine size of init design (!) 
  # TODO: determine lambda
  if (objective == "SO")
    file.copy("objective_SO.R", "objective.R")

  if (objective == "scalar")
    file.copy("objective_scalar.R", "objective.R")

  z = runBOCS(n_vars = p, n_init = ninit, eval_budget = maxeval, sim_anneal = sim_anneal, lamb = lambda)

  runtime = proc.time() - time

  # --- getting result in the right shape
  res = as.data.frame(rbind(z[[1]]$x_vals, z[[2]]))
  y = c(z[[1]]$y_vals, z[[3]])
  res$y = - y

  y.hout = c()
  y.train = c()

  for (i in 1:nrow(res)) {
      args = list()
      args$selector.selection = as.logical(res[i, ])
      learner = cpoSelector() %>>% checkLearner(lrn.tuned, type = getTaskType(train.task))      
      lrn.feat = setHyperPars2(learner, par.vals = args)
      model = train(lrn.feat, train.task)
      prd.model = predict(model, train.task)
      prd = predict(model, test.task)
      y.train = c(y.train, performance(prd.model)[1])
      y.hout = c(y.hout, performance(prd)[1])
  }
  
  res$hout = y.hout
  res$y.train = y.train

  return(list(result = res, task.test = task.test, task.train = task.train, runtime = runtime, tunetime = timetune))
} 

