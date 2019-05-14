# library("mlr")

# learner = "SVM"
# data = "data/sonar"
# maxeval = 15L
# infill = "cb"
# surrogate = "km.nugget"
# cv.iters = 2L
# ninit = 10L

# --- source the prob design
source("probdesign.R")
source("helpers.R")

OVERWRITE = TRUE

packages = c("batchtools", "ParamHelpers", "mlr", "mlrCPO", "mlrMBO")

datafolder = "data"

# Maximum number of evaluations allowed
MAXEVAL = 20L

# surrogate learner
SURROGATE = list(randomForest = cpoImputeConstant("__MISSING__") %>>% makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE, predict.type = "se"),
	              km.nugget = cpoDummyEncode() %>>% makeLearner("regr.km", predict.type = "se", par.vals = list(nugget.estim = TRUE, nugget.stability = 10e-8))
)

# infill criterion
INFILL = list("cb" = makeMBOInfillCritCB())

# number of cross-validation iterations
CV.ITERS = 2L

pdes = lapply(names(datasets), function(x) data.table(rinst.iter = 1:10))
names(pdes) = names(datasets)

# sim_anneal FALSE means that we use BOCS-SDP
# this performed best
# BOCS maximizes!!!! 
ades.BOCS = CJ(learner = c("SVM"), 
			maxeval = MAXEVAL, 
			cv.iters = CV.ITERS,
			sim_anneal = c(TRUE),
			lambda = c(10^(-4), 1),
			ninit = 8L, 
			sorted = FALSE)

ades.randomsearch = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			cv.iters = CV.ITERS,
			tune_hyperpars = c(TRUE, FALSE),
			sorted = FALSE)


ades.mboNaive = CJ(learner = c("SVM"), 
			maxeval = MAXEVAL, 
			cv.iters = CV.ITERS,
			infill = c("cb"),
			surrogate = c("km.nugget"),
			sorted = FALSE)


REPLICATIONS = 1L
