library(batchtools)
library(stringi)
library(dplyr)

resources.serial = list(
	walltime = 3600L * 48L, memory = 1024L * 2L,
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

resources.mpp2 = list(ncpus = 15L,
	walltime = 3600L * 48L, memory = 1000L * 50L,
	clusters = "mpp2") # get name from lrz homepage))


reg = loadRegistry("registry_test", writeable = TRUE)
tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval", "cv.iters", "sim_anneal", 
	"lambda", "ninit", "objective"))
tosubmit = tab[objective == "SO", ]

submitJobs(tosubmit, resources = resources.mpp2)

# --- LRZ ivymuc ---  