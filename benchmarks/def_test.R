## TODO: Benchmark design

packages = c("batchtools", 
	"ecr", "mobafeas",
	"magrittr", 
	"mosmafs", 
	"ParamHelpers", 
	"mlr", "mlrCPO", "mlrMBO", "reticulate")

# source the prob design
source("probdesign.R")

OVERWRITE = FALSE

datafolder = "data"

# Maximum number of evaluations allowed
MAXEVAL = 25L

# Infill optimizer
# TODO: do we want to compare here?
INFILL_OPT = list("mosmafs")

# Infill crit
INFILL = list("cb" = makeMBOInfillCritCB())

# Kernel
KERNELS = list(
    hamming = kernelMBFHamming(),
    graph = kernelMBFGraph(TRUE),
    graph.multi = kernelMBFGraph(FALSE),
    agreement = kernelMBFAgreement(FALSE),
    agreement.limited = kernelMBFAgreement(TRUE),
    agree.cor = kernelMBFAgreeCor(data$task, FALSE),
    agree.cor.limited = kernelMBFAgreeCor(data$task, TRUE))


# inner resampling iterations
# TODO: keep it that high?
CV.ITERS = 10L

# TODO: determine the size of the initial design
NINIT = 20L

# problem design gives the resampling iteration
pdes = lapply(names(datasets), function(x) data.table(rinst.iter = 1:10))
names(pdes) = names(datasets)

# need to choose lambda appropriately
ades.BOCS = CJ(learner = c("SVM"), 
			maxeval = MAXEVAL, 
			cv.iters = CV.ITERS,
			sim_anneal = c(TRUE),
			lambda = 0,
			ninit = NINIT,
			objective = c("SO", "scalar"), 
			sorted = FALSE)

ades.randomsearch = CJ(learner = c("SVM", "kknn", "xgboost"), 
			maxeval = MAXEVAL, 
			cv.iters = CV.ITERS,
			tune_hyperpars = c(TRUE, FALSE),
			sorted = FALSE)


ades.mobafeas = CJ(learner = c("SVM"), 
			maxeval = MAXEVAL, 
			cv.iters = CV.ITERS,
			infill.opt = "mosmafs",
			infill = c("cb"),
			kernel = "hamming",
			ninit = NINIT, 
			objective = c(TRUE),
			joint.hyperpars = c(TRUE, FALSE),
			sorted = FALSE)


REPLICATIONS = 1L

