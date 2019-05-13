## TODO: Benchmark design

packages = c("batchtools", "ecr", "magrittr", "mosmafs", "ParamHelpers", "mlr", "mlrCPO", "mlrMBO")

# source the prob design
source("probdesign.R")

OVERWRITE = FALSE

datafolder = "data"

# do not overwrite registry
OVERWRITE = FALSE

# Maximum number of evaluations allowed
# TODO: 2000 sufficient?
MAXEVAL = 4000L

# Infill optimizer


# Infill crit
INFILL = list("cb" = makeMBOInfillCritCB())


# Surrogate
SURROGATE = list(randomForest = cpoImputeConstant("__MISSING__") %>>% makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE, predict.type = "se"),
	              km.nugget = cpoDummyEncode() %>>% makeLearner("regr.km", predict.type = "se", par.vals = list(nugget.estim = TRUE, nugget.stability = 10e-8))
)


ades.BOCS = list()

ades.SMAC = list()

# TODO: discuss with martin what we will test
# Different kernels? 
ades.mobafeas = list()

# We get random search for free
ades.random = list()

# We get mosmafs for free
ades.mosmafs = list()

# TODO: need to check how we get there
ades.graphs = list()


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

