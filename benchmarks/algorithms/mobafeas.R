mobafeas = function(data, job, instance, learner, maxeval, infill, infill.opt, surrogate, cv.iters, ninit, obejctive) {

  # --- task and learner ---
  train.task = instance$train.task
  test.task = instance$test.task # for outer evaluation

  lrn = LEARNERS[[learner]]

  # --- inner resampling ---
  stratcv = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)

  # --- number of features ---
  p = getTaskNFeats(train.task)
  ps = PAR.SETS[[learner]]

  if (is.null(ps)) {
   obj = makeMobafeasObjective(learner = lrn, task = train.task, resampling = stratcv,
    multi.objective = OBJECTIVES[[objective]], holdout.data = test.task)   
  } else {
   obj = makeMobafeasObjective(learner = lrn, task = train.task, resampling = stratcv,
    ps = ps, multi.objective = OBJECTIVES[[objective]], holdout.data = test.task)   
  }

  ps.obj = getParamSet(obj)

  # configure infill optimization
  mutator.simple = combine.operators(ps.obj,
    numeric = ecr::setup(mutGaussScaled, sdev = 0.1),
    integer = ecr::setup(mutGaussIntScaled, sdev = 0.1),
    selector.selection = mutBitflipCHW)

  crossover.simple = combine.operators(ps.obj,
    numeric = recPCrossover,
    integer = recPCrossover,
    selector.selection = recPCrossover)

  mosmafs.config = MosmafsConfig(mutator.simple, crossover.simple, list(mosmafsTermStagnationObjStatistic(3)))

  surrogate = constructMBFLearner(ps.obj, kernelMBFHamming())
  
  ctrl = makeMBFControl(mosmafs.config) %>% setMBOControlTermination(maxeval)

  initials = sampleValues(ps.obj, ninit, discrete.names = TRUE) %>% initSelector()

  time = proc.time()

  # parallelStartMulticore(cpus = 15L)
  result = mobafeasMBO(obj, initials, surrogate, ctrl, show.info = TRUE)

  # parallelStop()

  runtime = proc.time() - time


  return(list(result = result, task.test = task.test, task.train = task.train, runtime = runtime))
}
