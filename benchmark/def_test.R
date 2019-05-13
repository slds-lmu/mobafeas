# library("mlr")

# learner = "SVM"
# data = "data/sonar"
# maxeval = 15L
# infill = "cb"
# surrogate = "km.nugget"
# cv.iters = 2L
# ninit = 10L

OVERWRITE = FALSE

packages = c("batchtools", "ParamHelpers", "mlr", "mlrCPO", "mlrMBO")

# --- source the prob design
source("probdesign.R")
source("helpers.R")

datafolder = "data"

# Maximum number of evaluations allowed
MAXEVAL = 100L

# surrogate learner
SURROGATE = list(randomForest = cpoImputeConstant("__MISSING__") %>>% makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE, predict.type = "se"),
	              km.nugget = cpoDummyEncode() %>>% makeLearner("regr.km", predict.type = "se", par.vals = list(nugget.estim = TRUE, nugget.stability = 10e-8))
)

# infill criterion
INFILL = list("cb" = makeMBOInfillCritCB())

# number of cross-validation iterations
CV.ITERS = 2L


# sim_anneal FALSE means that we use BOCS-SDP
# this performed best
# BOCS maximizes!!!! 
ades.BOCS = CJ(learner = c("SVM"), 
			maxeval = MAXEVAL, 
			cv.iters = CV.ITERS,
			sim_anneal = c(FALSE),
			lambda = c(10^(-4), 1),
			ninit = 8L, 
			sorted = FALSE)

ades.randomsearch = CJ(learner = c("SVM"), 
			maxeval = MAXEVAL, 
			cv.iters = CV.ITERS,
			sorted = FALSE)


ades.mboNaive = CJ(learner = c("SVM"), 
			maxeval = MAXEVAL, 
			cv.iters = CV.ITERS,
			infill = c("cb"),
			surrogate = c("km.nugget"),
			sorted = FALSE)


REPLICATIONS = 1L
