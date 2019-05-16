## TODO: Benchmark design

packages = c("batchtools", "ParamHelpers", "mlr", "OpenML", "parallelMap")

# source the prob design
source("probdesign.R")

OVERWRITE = FALSE

datafolder = "data"

# Maximum number of evaluations allowed
# 200 * p
MAXEVALF = 40L

CVITERS = 5L

ades.random = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVALF, 
			cv.iters = CVITERS,
			sorted = FALSE)

REPLICATIONS = 1L

