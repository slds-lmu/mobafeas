## TODO: Benchmark design

packages = c("batchtools", "ecr", "magrittr", "mosmafs", "ParamHelpers", "mlr", "mlrCPO", "mlrMBO")

# source the prob design
source("../probdesign.R")

OVERWRITE = FALSE

datafolder = "data"

# do not overwrite registry
OVERWRITE = FALSE

# Maximum number of evaluations allowed
MAXEVAL = 1000L

# Infill optimizer


# Infill crit
INFILL = list("cb" = makeMBOInfillCritCB())

# Surrogate
SURROGATE = list(randomForest = cpoImputeConstant("__MISSING__") %>>% makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE, predict.type = "se"),
	              km.nugget = cpoDummyEncode() %>>% makeLearner("regr.km", predict.type = "se", par.vals = list(nugget.estim = TRUE, nugget.stability = 10e-8))
)


pdes = lapply(names(datasets), function(x) data.table(rinst.iter = 1:10))
names(pdes) = names(datasets)

ades.BOCS = list()

ades.random = list()

ades.MBOnaive = list()

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

