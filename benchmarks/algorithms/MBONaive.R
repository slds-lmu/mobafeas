# ---
# NAIVE MBO
# ---

naiveMBO = function(data, job, instance, learner, cv.iters, ninit, objective, 
  feature.sel, filter.methods, maxeval, maxtime, parallelize) {

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
  # 2. Handling of feature selection 
  # --- 

  # a) No feature selection at all
  if (feature.sel == "none")

  # b) Fixed filter + tune percentage
  if (feature.sel == "perc") {
      filter.method = filter.methods[[1]] # we take the first filter by default
      fv = generateFilterValuesData(train.task, method = filter.method)
      ps = c(ps, makeNumericParam("fw.perc", lower = 0, upper = 1))
  }

  # c) Tune filter + threshold 
  if (feature.sel == "type.perc") {
      filter.method = filter.methods
      ps = c(ps, makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1), 
                 makeDiscreteParam("filter.method", values = filter.methods)))
  }

  # ---
  # 3. Setup objective 
  # --- 


  if (objective == "MO") {
     obj = makeMultiObjectiveFunction(name = "MOtuning",
          fn = function(x) {
            filtered.task = train.task
             if (feature.sel == "perc") {
                  filtered.task = filterFeatures(train.task, fval = fv, perc = x$fw.perc)
              }
              if (feature.sel == "type.perc") {
                fv = generateFilterValuesData(train.task, method = x&filter.method)
                filtered.task = filterFeatures(train.task, fval = fv, perc = x$fw.perc)                   
              }
            lrn2 = setHyperPars2(lrn, x[- which(names(x) %in% c("fw.perc", "filter.method"))])

            res = resample(lrn2, filtered.task, inner, show.info = FALSE)$aggr

            return (c(res, getTaskNFeats(filtered.task)))

          },
          par.set = ps,
          n.objectives = 2L,
          has.simple.signature = FALSE,
          minimize = c(TRUE, TRUE)
        )
   } else {
        obj = makeSingleObjectiveFunction(name = "singleobjtuning",
                fn = function(x) {
                  filtered.task = train.task
                  if (feature.sel == "perc") {
                      filtered.task = filterFeatures(train.task, fval = fv, perc = x$fw.perc)
                  }
                  if (feature.sel == "type.perc") {
                    fv = generateFilterValuesData(train.task, method = x&filter.method)
                    filtered.task = filterFeatures(train.task, fval = fv, perc = x$fw.perc)                   
                  }

                  lrn2 = setHyperPars2(lrn, x[- which(names(x) == "fw.perc")])

                  res = resample(lrn2, filtered.task, inner, show.info = FALSE)$aggr

                  if (objective == "SO")
                    return (res)

                  if (objective == "scalar")
                    return (res + getTaskNFeats(filtered.task) / getTaskNFeats(train.task))
                },
                par.set = ps,
                noisy = TRUE,
                has.simple.signature = FALSE,
                minimize = TRUE
              )

   }


  des = generateDesign(n = ninit, par.set = getParamSet(obj), fun = lhs::randomLHS)

  ctrl = makeMBOControl(n.objectives = getNumberOfObjectives(obj), store.model.at = 1L) 
  if (objective == "MO"){
      ctrl = setMBOControlMultiObj(ctrl, method = "parego", ref.point.val = c(1, 1))
    }
  else 
    ctrl = setMBOControlInfill(ctrl, crit.cb)

  ctrl = setMBOControlTermination(ctrl, max.evals = maxeval)


  time = proc.time()

  result = mbo(obj, design = des, control = ctrl)


  runtime = proc.time() - time

  if (parallelize)
    parallelMap::parallelStop()

  return(list(result = result, task.test = test.task, task.train = train.task, runtime = runtime))
}
