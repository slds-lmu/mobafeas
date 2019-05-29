mobafeas = function(data, job, instance, learner, maxeval, infill, infill.opt, cv.iters, 
  ninit, objective, kernel, joint.hyperpars) {

  # --- task and learner ---
  train.task = instance$train.task
  test.task = instance$test.task # for outer evaluation

  lrn = LEARNERS[[learner]]

  # --- inner resampling ---
  inner = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)

  # --- number of features ---
  p = getTaskNFeats(train.task)
  ps = PAR.SETS[[learner]]

  timetune = NA

  if (!joint.hyperpars) {
    # --- do randomsearch
      tune.iters = maxeval
      ctrl = makeTuneControlRandom(maxit = tune.iters)
      
      lrn.wrp = makeTuneWrapper(lrn, resampling = inner, 
        par.set = ps, control = ctrl, show.info = FALSE)

      # tune the tuning
      time = proc.time()

      # parallelMap::parallelStartMulticore(cpus = 8L) 
      # parallelMap::parallelStartSocket(cpus = 8L) 
      
      mod = train(lrn.wrp, train.task)

      tuneres = getTuneResult(mod)
      lrn = setHyperPars(lrn, par.vals = tuneres$x)
      
      timetune = proc.time() - time

      ps = pSS()
  }

  mfo = makeMobafeasObjective(lrn, train.task, ps, inner, holdout.data = test.task,
      multi.objective = OBJECTIVES[[objective]])

  ps.obj = getParamSet(mfo)

  mutator.simple = combine.operators(ps.obj,
    numeric = ecr::setup(mutGaussScaled, sdev = 0.1),
    integer = ecr::setup(mutGaussIntScaled, sdev = 0.1),
    selector.selection = mutBitflipCHW)

  crossover.simple = combine.operators(ps.obj,
    numeric = recPCrossover,
    integer = recPCrossover,
    selector.selection = recPCrossover)

  mosmafs.config = MosmafsConfig(mutator.simple, crossover.simple,
    list(mosmafsTermStagnationObjStatistic(3)))

  surrogate = constructMBFLearner(ps.obj, KERNELS[[kernel]])
  
  ctrl = makeMBFControl(mosmafs.config, infill.crit = INFILL[[infill]]) %>% setMBOControlTermination(maxeval - ninit) 
  # ctrl$infill.crit = INFILL[[infill]]

  initials = sampleValues(ps.obj, ninit, discrete.names = TRUE) %>% initSelector()

  time = proc.time()

  # parallelStartMulticore(cpus = 15L)
  result = mobafeasMBO(mfo, initials, surrogate, ctrl, show.info = TRUE)

  # parallelStop()

  runtime = proc.time() - time

  return(list(result = result, task.test = test.task, task.train = train.task, runtime = runtime, tunetime = timetune))
}
