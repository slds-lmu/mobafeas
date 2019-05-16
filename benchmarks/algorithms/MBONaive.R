MBONaive = function(data, job, instance, learner, maxeval, infill, infill.opt, surrogate, cv.iters, ninit) {

  # --- task and learner ---
  train.task = instance$train.task
  test.task = instance$test.task # for outer evaluation

  lrn = LEARNERS[[learner]]

  # --- inner resampling ---
  stratcv = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)

  # --- number of features ---
  p = getTaskNFeats(task.train)


  # --- create baseline objective
  # REPLACE BY MARTINS CODE?
  obj = makeSingleObjectiveFunction(name = "MBO",
    fn = function(x) {
      sstask = subsetTask(task.train, features = x == 1L)
      r = resample(lrn, sstask, stratcv)
      as.numeric(r$aggr[1])
    },
    par.set = makeParamSet(
        makeIntegerVectorParam("x", 0L, 1L, len = p)
        # makeLogicalVectorParam("x", len = getTaskNFeats(task.train))
      )
  )

  des = generateDesign(n = ninit(task.train), par.set = getParamSet(obj), fun = lhs::randomLHS)
  des$y = apply(des, 1, obj)

  ctrl = makeMBOControl(n.objectives = 1) 
  ctrl = setMBOControlInfill(ctrl, INFILL[[infill]])
  ctrl = setMBOControlTermination(ctrl, max.evals = maxeval)

  time = proc.time()

  # parallelStartMulticore(cpus = 15L)
  result = mbo(obj, design = des, control = ctrl, learner = SURROGATE[[surrogate]])

  # parallelStop()

  runtime = proc.time() - time


  return(list(result = result, task.test = task.test, task.train = task.train, runtime = runtime))
}
