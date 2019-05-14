# TODO: UNIFORM INITIALIZATION
# TODO: RESULT OBJECT SHOULD BE APPROPRIATE
# TODO: TUNING OVER HYPERPARS

randomsearch = function(data, job, instance, learner, maxeval, cv.iters, tune_hyperpars) {

  # --- task and learner ---
  train.task = instance$train.task
  test.task = instance$test.task # for outer evaluation

  lrn = LEARNERS[[learner]]

  # --- inner resampling ---
  stratcv = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)

  p = getTaskNFeats(train.task)

  # --- paramset features ---
  ps = makeParamSet(
        makeIntegerVectorParam("x", 0L, 1L, len = p)
      )

  # --- paramset for hyperparameters---
  if (tune_hyperpars){
    ps = c(ps, PAR.SETS[[learner]])
  }
 
  # --- initialization ---
  # --- TODO: Uniform Initialization  
  design = generateRandomDesign(maxeval, ps)

  # --- TODO: SET HYPERPARAMETERS (Martins implementation)
  obj = makeSingleObjectiveFunction(name = "MBO",
    fn = function(x) {
      sstask = subsetTask(train.task, features = x == 1L)
      r = resample(lrn, sstask, stratcv)
      as.numeric(r$aggr[1])
    },
    par.set = ps
  )

  time = proc.time()

  design$y = apply(design, 1, obj)
  design$nfeat = apply(design[, 1:p], 1, sum)

  runtime = proc.time() - time

  # --- TODO: Result Object
  # result = makeMBOResult(design = design, p = p)

  return(list(design = design, test.task = test.task, train.task = train.task, runtime = runtime))
} 