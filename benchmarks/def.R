## TODO: Benchmark design

packages = c("batchtools", "ecr", "magrittr", "mosmafs", "ParamHelpers", "mlr", "mlrCPO", "mlrMBO")

# source the prob design
source("probdesign.R")

OVERWRITE = FALSE

datafolder = "data"

# do not overwrite registry
OVERWRITE = FALSE

# Maximum number of evaluations allowed
MAXEVAL = 1000L

# Infill optimizer
# TODO: do we want to compare here?
INFILL_OPT = list("mosmafs", "focussearch")

# Infill crit
INFILL = list("cb" = makeMBOInfillCritCB())

# Surrogate
SURROGATE = list(randomForest = cpoImputeConstant("__MISSING__") %>>% makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE, predict.type = "se"),
	              km.nugget = cpoDummyEncode() %>>% makeLearner("regr.km", predict.type = "se", par.vals = list(nugget.estim = TRUE, nugget.stability = 10e-8)))


# Kernel
KERNEL = list("naive" = NA)

# inner resampling iterations
# TODO: keep it that high?
CV.ITERS = 10L

# TODO: determine the size of the initial design
NINIT = function(task) getTaskNFeats(task)

# problem design gives the resampling iteration
pdes = lapply(names(datasets), function(x) data.table(rinst.iter = 1:10))
names(pdes) = names(datasets)

# need to choose lambda appropriately
ades.BOCS = CJ(learner = c("SVM"), 
			maxeval = MAXEVAL, 
			cv.iters = CV.ITERS,
			sim_anneal = c(TRUE),
			lambda = c(10^(-2)),
			ninit = NINIT, 
			sorted = FALSE)

ades.randomsearch = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			cv.iters = CV.ITERS,
			tune_hyperpars = c(TRUE, FALSE),
			sorted = FALSE)


ades.MBO = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			cv.iters = CV.ITERS,
			infill.opt = "mosmafs",
			infill = c("cb"),
			ninit = NINIT, 
			surrogate = c("km.nugget"),
			sorted = FALSE)


REPLICATIONS = 10L

# ades.random = CJ(learner = c("SVM", "kknn", "xgboost"), 
# 			maxeval = MAXEVAL, 
# 			filter = c("none", "custom"),
# 			initialization = c("none", "unif"), 
# 			sorted = FALSE)

# ades.mosmafs = CJ(learner = c("xgboost"), 
# 			maxeval = MAXEVAL, 
# 			filter = c("none", "custom"),
# 			initialization = c("none", "unif"), 
# 			lambda = 15L,
# 			mu = 80,
# 			parent.sel = c("selTournamentMO"),
# 			chw.bitflip = c(FALSE, TRUE),
# 			adaptive.filter.weights = c(FALSE, TRUE),
# 			filter.during.run = c(FALSE, TRUE),
# 			sorted = FALSE)

