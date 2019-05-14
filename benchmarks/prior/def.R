## TODO: Benchmark design

packages = c("batchtools", "ParamHelpers", "mlr", "OpenML")

# source the prob design
source("probdesign.R")

OVERWRITE = FALSE

datafolder = "data"

# Maximum number of evaluations allowed
# 40 * p
MAXEVALF = 4L

CVITERS = 2L

ades.random = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVALF, 
			cv.iters = CVITERS,
			sorted = FALSE)

REPLICATIONS = 1L

