library(batchtools)
library(stringi)
library(dplyr)

resources.serial = list(
	walltime = 3600L * 72L, memory = 1024L * 4L,
	clusters = "serial" # get name from lrz homepage)
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


reg = loadRegistry("registry", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval", "cv.iters", "sim_anneal", 
	"lambda", "ninit", "objective", "kernel", "joint.hyperpars", "parallelize"))
tosubmit = tab[algorithm == "BOCS", ]

# --- choose what we want to run now
tosubmit = ijoin(tosubmit, findNotDone())
tosubmit = tosubmit[maxeval == 500, ]
tosubmit$chunk = rep(1:(nrow(tosubmit) / 3L), 3L)

submitJobs(381:740, resources = resources.mpp2)

# --- LRZ ivymuc ---  