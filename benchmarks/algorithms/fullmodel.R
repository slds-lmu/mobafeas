# ---
# MOBAFEAS ALGORITHM
# ---


mobafeas = function(data, job, instance, learner, cv.iters, maxeval, parallelize) {

  # ---
  # 0. Define task, learner, and inner resampling
  # ---

  lrn = LEARNERS[[learner]] # learner 
  ps = PAR.SETS[[learner]] # learner parameters

  train.task = instance$train.task # training
  test.task = instance$test.task # for outer evaluation
  p = getTaskNFeats(train.task)


  inner = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)


  # ---
  # 1. eventually setup parallel environemnt
  # --- 
  if (parallelize)
    parallelMap::parallelStartMulticore(cpus = 10L)

  # ---
  # 2. As baseline consider the case of no joint optimization
  # --- 

  tune.iters = maxeval # number of tuning iterations = number of maximum evaluations

  ctrl = makeTuneControlRandom(maxit = tune.iters) 
    
  lrn.wrp = makeTuneWrapper(lrn, resampling = inner, par.set = ps, control = ctrl, show.info = FALSE)

  time = proc.time()
    
  mod = train(lrn.wrp, train.task)

  tuneres = getTuneResult(mod)
  
  lrn = setHyperPars(lrn, par.vals = tuneres$x)
    
  timetune = proc.time() - time

  config = data.frame(matrix(rep(0, p), nrow = 1))
  names(config) = paste("selector.selection", 1:p, sep = "")

  config = cbind(do.call(cbind, tuneres$x), config)
  config$y = tuneres$y

  dummyFun = makeMobafeasObjective(lrn, train.task, ps, inner, holdout.data = test.task)
  dummyLrn = constructMBFLearner(getParamSet(dummyFun))
  dummyCtrl = makeMBOControl()

  population = sampleValues(getParamSet(dummyFun), 1)
  df = listToDf(population, getParamSet(dummyFun))
  

  res = mbo(fun = dummyFun, design = config, learner = dummyLrn, control = dummyCtrl)









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
