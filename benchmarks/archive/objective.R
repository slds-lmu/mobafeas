library(mlr)

myArgs = commandArgs(trailingOnly = FALSE, asValue = FALSE)

print(myArgs)

as.list(myArgs)

# --- read task, learner, resampling
task = readRDS("tasktrain.rds")
lrn = readRDS("lrn.rds")
stratcv = readRDS("stratcv.rds")


# --- subset Task with inputs
feats = as.logical(as.numeric(myArgs[2:length(myArgs)]))
sstask = subsetTask(task, features = feats)

# --- perform resampling 
r = resample(lrn, sstask, stratcv)

# --- cat will write the result to the stdout stream
# --- negative because BOC maximizes
cat(as.numeric(- r$aggr[1]) - sum(feats))
