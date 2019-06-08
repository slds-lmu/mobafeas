mobafeas = function(data, job, instance, 
  learner, infill, cv.iters, ninit, initialization,
  objective, kernel, joint.hyperpars, 
  maxeval, maxtime,
  parallelize) {

  # --- task and learner ---
  train.task = instance$train.task
  test.task = instance$test.task # for outer evaluation
  lrn = LEARNERS[[learner]]

  # --- parameter set of the learner ---
  ps = PAR.SETS[[learner]]

  # --- inner resampling ---
  inner = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)

  if (parallelize)
    parallelMap::parallelStartMulticore(cpus = 10L)

  # --- do randomsearch to choose hyperpars separately
  timetune = NA
  if (!joint.hyperpars) {
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
  
  ctrl = makeMBFControl(mosmafs.config, infill.crit = INFILL[[infill]]) %>% setMBOControlTermination(iters = maxeval - ninit, time.budget = maxtime) 

  # --- initialization ---
  initials =  sampleValues(ps.obj, ninit, discrete.names = TRUE) 

  if (initialization == "unif") {
    distribution = function() floor(runif(1, 0, length(initials[[1]]$selector.selection) + 1))
    initials = initSelector(initials, distribution = distribution)
  }

  time = proc.time()

  result = mobafeasMBO(mfo, initials, surrogate, ctrl, show.info = TRUE)

  runtime = proc.time() - time

  if (parallelize)
    parallelMap::parallelStop()

  return(list(result = result, task.test = test.task, task.train = train.task, runtime = runtime, tunetime = timetune))
}
