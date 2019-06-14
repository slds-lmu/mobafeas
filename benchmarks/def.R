
packages = c("batchtools", "ecr", "mobafeas",
	"magrittr", "mosmafs", "ParamHelpers", "parallelMap",
	"mlr", "mlrCPO", "mlrMBO", "reticulate")

# source the prob design
source("probdesign.R")

OVERWRITE = FALSE

datafolder = "data"

# Maximum number of evaluations allowed
MAXEVAL = 120L

# Maximum time 
MAXTIME = 48 * 3600L

# Infill optimizer
# TODO: do we want to compare here?
INFILL_OPT = list("mosmafs")

# Infill crit
INFILL = list("cb" = InfillCB(), "ei" = InfillEI())

# Kernel
KERNELS = list(
    hamming = kernelMBFHamming(),
    # graph = kernelMBFGraph(TRUE),
    graph.limited = kernelMBFGraph(FALSE),
    agreement = kernelMBFAgreement(FALSE),
    agreement.limited = kernelMBFAgreement(TRUE)# ,
    # agree.cor = kernelMBFAgreeCor(data$task, FALSE),
    # agree.cor.limited = kernelMBFAgreeCor(data$task, TRUE)
    )


# inner resampling iterations
# TODO: keep it that high?
CV.ITERS = 10L

# TODO: determine the size of the initial design
NINIT = 100L

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
			parallelize = FALSE, 
			initialization = c("unif", "binom"),
			sorted = FALSE)


ades.mobafeas = CJ(learner = c("SVM"), 
			maxeval = MAXEVAL, 
			maxtime = MAXTIME,
			cv.iters = CV.ITERS,
			infill = c("cb", "ei"),
			kernel = c("hamming", "graph.limited", "agreement"),
			ninit = NINIT, 
			objective = c("SO", "MO", "scalar"),
			parallelize = FALSE,
			joint.hyperpars = c(TRUE, FALSE),
			initialization = c("unif", "binom"),
			sorted = FALSE)


REPLICATIONS = 1L

