# TODO: RESULT OBJECT SHOULD BE APPROPRIATE

BOCS = function(data, job, instance, learner, initialization,
  maxeval, maxtime, cv.iters, sim_anneal, lambda, ninit,
  objective, parallelize) {

  # --- 0. Reading stuff

  # --- task and learner ---
  train.task = instance$train.task
  test.task = instance$test.task # for outer evaluation
  lrn = LEARNERS[[learner]]

  # --- inner resampling ---
  inner = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)

  # --- parameter set of the learner ---
  ps = PAR.SETS[[learner]]

  if (parallelize)
    parallelMap::parallelStartMulticore(cpus = 10L)

  # --- 1. do randomsearch to choose hyperpars separately
  timetune = NA
  tune.iters = maxeval
  ctrl = makeTuneControlRandom(maxit = tune.iters)
    
  lrn.wrp = makeTuneWrapper(lrn, resampling = inner, 
      par.set = ps, control = ctrl, show.info = FALSE)

  # tune the tuning
  time = proc.time()
    
  mod = train(lrn.wrp, train.task)

  tuneres = getTuneResult(mod)
  lrn = setHyperPars(lrn, par.vals = tuneres$x)
    
  timetune = proc.time() - time

  ps = pSS()

  # --- 2. Start optimization
  setwd("BOCS/BOCSpy")
  BOCS = import_from_path("BOCS")
  setwd("../../")

  inputs = list()
  inputs$n_vars = getTaskNFeats(train.task)
  inputs$evalBudget = maxeval
  inputs$n_init = ninit
  inputs$lambda = lambda

  if (objective == "scalar") {
    inputs$model = function(x) {
      
      # --- subset Task with inputs
      feats = as.logical(x)
      sstask = subsetTask(train.task, features = feats)

      # --- perform resampling 
      r = resample(lrn, sstask, inner, show.info = TRUE)
      as.numeric(r$aggr[1] + mean(feats)) 
    }
  } else {
      inputs$model = function(x) {
      
      # --- subset Task with inputs
      feats = as.logical(x)
      sstask = subsetTask(train.task, features = feats)

      # --- perform resampling 
      r = resample(lrn, sstask, inner, show.info = TRUE)
      as.numeric(r$aggr[1]) 
    }
  }

  inputs$penalty = function(x) inputs$lambda * apply(x, 1, sum)

  # Generate initial samples for statistical models
  
  if (initialization == "unif") {
    initials =  sampleValues(pSS(selector.selection = NA:integer[0, 1]^getTaskNFeats(train.task)), ninit, discrete.names = TRUE) 
    distribution = function() floor(runif(1, 0, length(initials[[1]]$selector.selection) + 1))
    initials = initSelector(initials, distribution = distribution)
    inputs$x_vals = do.call(rbind, lapply(initials, function(x) x$selector.selection))
 } else {
    inputs$x_vals  = BOCS$sample_models(inputs$n_init, inputs$n_vars)
  }

  inputs$y_vals  = apply(inputs$x_vals, 1, inputs$model)

  time = proc.time()

  z = BOCS$BOCS(inputs, 2L, 'SA')

  runtime = proc.time() - time

  mfo = makeMobafeasObjective(lrn, train.task, ps, makeResampleDesc("Holdout"), holdout.data = test.task,
        multi.objective = FALSE)  

  design = data.frame(rbind(inputs$x_vals, z[[1]]))
  names(design) = paste("selector.selection", 1:getTaskNFeats(train.task), sep = "")
  # design$y = c(inputs$y_vals, z[[2]])

  # workaround to get the proper result object
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = 1)

  res = mbo(fun = mfo, learner = makeLearner("regr.randomForest", predict.type = "se"), design = design, control = ctrl)

  if (parallelize)
    parallelMap::parallelStop()


  return(list(result = res, task.test = test.task, task.train = train.task, runtime = runtime, tunetime = timetune, y = y))
} 


library(mlrMBO)
fn = makeSingleObjectiveFunction(
  name = "test", 
  fn = function(x) {
    res = sum(x^2)
    setAttribute(res, "extras", list(.foo = runif(1)))
  }, 
  par.set = makeNumericParamSet(id = "x", len = 2, -2, 2))
ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = 3)
des = generateDesign(n = 10L, par.set = getParamSet(fn))
res1 = mbo(fn, design = des, control = ctrl)
#works fine
des$y = apply(des,1,fn)
res2 = mbo(fn, design = des, control = ctrl)
as.data.frame(res2$opt.path)
getOptPathEl(res$opt.path, 16)
