MBONaive = function(data, job, instance, learner, maxeval, infill, surrogate, cv.iters, ninit) {

  id = strsplit(data, "/")[[1]][2]

  # --- task and learner ---
  train.data = readRDS(file.path(data, "train.arff.rds"))
  task.train = makeClassifTask(id = id, data = train.data, target = "class")
  test.data = readRDS(file.path(data, "test.arff.rds"))  
  task.test = makeClassifTask(id = id, data = test.data, target = "class")
  
  lrn = LEARNERS[[learner]]

  # --- inner resampling ---
  stratcv = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)

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

  des = generateDesign(n = 2 * p, par.set = getParamSet(obj), fun = lhs::randomLHS)
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
