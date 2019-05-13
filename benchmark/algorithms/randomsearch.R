randomsearch = function(data, job, instance, learner, maxeval, cv.iters) {

  id = strsplit(data, "/")[[1]][2]

  # --- task and learner ---
  train.data = readRDS(file.path(data, "train.arff.rds"))
  task.train = makeClassifTask(id = id, data = train.data, target = "class")
  test.data = readRDS(file.path(data, "test.arff.rds"))  
  task.test = makeClassifTask(id = id, data = test.data, target = "class")
  
  lrn = LEARNERS[[learner]]

  p = getTaskNFeats(task.train)

  # --- paramset ---
  ps = makeParamSet(
        makeIntegerVectorParam("x", 0L, 1L, len = p)
      )

  # --- inner resampling ---
  stratcv = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)
  
  # --- initialization ---
  design = generateRandomDesign(maxeval, ps)

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

  time = proc.time()

  design$y = apply(design, 1, obj)

  runtime = proc.time() - time

  result = makeMBOResult(design = design, p = p)

  return(list(result = result, task.test = task.test, task.train = task.train, runtime = runtime))
} 
