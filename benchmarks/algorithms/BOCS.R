BOCS = function(data, job, instance, learner, maxeval, cv.iters, sim_anneal, lambda, ninit) {

  id = strsplit(data, "/")[[1]][2]

  # --- task and learner ---
  train.data = readRDS(file.path(data, "train.arff.rds"))
  task.train = makeClassifTask(id = id, data = train.data, target = "class")
  test.data = readRDS(file.path(data, "test.arff.rds"))  
  task.test = makeClassifTask(id = id, data = test.data, target = "class")
  
  lrn = LEARNERS[[learner]]

  # --- inner resampling ---
  stratcv = makeResampleDesc("CV", iters = cv.iters, stratify = TRUE)
  
  # --- we do not tune over parameters ---
  setwd("BOCS/BOCSpy")
  source_python("../../runBOCS.py")

  saveRDS(lrn, "lrn.rds")
  saveRDS(task.train, "tasktrain.rds")
  saveRDS(stratcv, "stratcv.rds")

  p = getTaskNFeats(task.train)

  time = proc.time()

  # TODO: determine size of init design (!) 
  z = runBOCS(n_vars = p, n_init = ninit, eval_budget = maxeval, sim_anneal = sim_anneal, lamb = lambda)

  runtime = proc.time() - time

  # --- workaround to get everything in the right "shape"
  design = as.data.frame(rbind(z[[1]]$x_vals, z[[2]]))
  names(design) = paste("x", 1:ncol(design), sep = "")
  y = c(z[[1]]$y_vals, z[[3]])
  design$y = y
  
  result = makeMBOResult(design, ninit = ninit, p)

  return(list(result = result, task.test = task.test, task.train = task.train, runtime = runtime))
}