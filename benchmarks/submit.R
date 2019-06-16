library(batchtools)
library(stringi)
library(dplyr)

resources.serial = list(
	walltime = 3600L * 72L, memory = 1024L * 4L,
	clusters = "serial", # get name from lrz homepage)
	max.concurrent.jobs = 1000L
)

resources.ivymuc = list(ncpus = 15L, 
	walltime = 3600L * 72L, memory = 1024L * 50L,
	clusters = "ivymuc") # get name from lrz homepage))

resources.teramem = list(ncpus = 15L,
	walltime = 3600L * 48L, memory = 1024L * 200L,
	clusters = "inter",
	partition = "teramem_inter") # get name from lrz homepage))

resources.mpp3 = list(ncpus = 15L,
	walltime = 3600L * 48L, memory = 1000L * 50L,
	clusters = "mpp3") # get name from lrz homepage))

resources.mpp2 = list(ncpus = 10L,
	walltime = 3600L * 48L, memory = 1000L * 50L,
	clusters = "mpp2") # get name from lrz homepage))

reg = loadRegistry("registry2", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval", "cv.iters", "sim_anneal", "initialization",
	"lambda", "ninit", "objective", "kernel", "joint.hyperpars", "parallelize"))
tosubmit = tab[algorithm == "BOCS" & maxeval == 30 & initialization == "unif" & objective == "SO", ]

# according to the paper, simmulated annealing is much faster
# thus we will use simmulated annealing
alogorithms = list(
	SO.BOCS.baseline = data.table(algorithm = "BOCS", sim_anneal = TRUE, initialization = "binom", lambda = 0, objective = "SO"),
	SO.BOCS.uniform = data.table(algorithm = "BOCS", sim_anneal = TRUE, initialization = "unif", lambda = 0, objective = "SO"),
	scalar.BOCS.baseline = data.table(algorithm = "BOCS", sim_anneal = TRUE, initialization = "binom", lambda = 0, objective = "scalar"),
	scalar.BOCS.uniform = data.table(algorithm = "BOCS", sim_anneal = TRUE, initialization = "unif", lambda = 0, objective = "scalar"),




	OI = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIFi = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIFiFm = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = FALSE, filter.during.run = TRUE),
	OIFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = FALSE, adaptive.filter.weights = TRUE, filter.during.run = TRUE),
	OIH = data.table(algorithm = "mosmafs", filter = "none", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = FALSE, filter.during.run = FALSE),
	OIHFiFmS = data.table(algorithm = "mosmafs", filter = "custom", initialization = "unif", chw.bitflip = TRUE, adaptive.filter.weights = TRUE, filter.during.run = TRUE),
	RS = data.table(algorithm = "randomsearch", initialization = "none", filter = "none", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA),
	RSI = data.table(algorithm = "randomsearch", initialization = "unif", filter = "none", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA),
	RSIF = data.table(algorithm = "randomsearch", initialization = "unif", filter = "custom", chw.bitflip = NA, adaptive.filter.weights = NA, filter.during.run = NA)
	)









# --- choose what we want to run now
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[- which(job.id %in% findOnSystem()$job.id), ]
tosubmit$chunk = rep(1:(nrow(tosubmit) / 2L), 2L)

submitJobs(tosubmit[1, ], resources = resources.serial)
testJob(4957)

# --- LRZ ivymuc ---  