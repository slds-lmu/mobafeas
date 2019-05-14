library(batchtools)
library(stringi)
library(dplyr)

reg = loadRegistry("registry", writeable = TRUE)

tab = summarizeExperiments(by = c("job.id", "algorithm", 
	"problem", "learner", "maxeval"))

resources.serial = list(
	walltime = 3600L * 48L, memory = 1024L * 4L,
	clusters = "serial", max.concurrent.jobs = 1000L # get name from lrz homepage)
)


tosubmit = ijoin(tab, findNotDone())

chunk.size = 6L
tosubmit$chunk = 1
nchunks = nrow(tosubmit) / chunk.size
tosubmit$chunk = rep(1:nchunks, each = chunk.size)

submitJobs(tosubmit, resources = resources.serial)
