# for each dataset we want to store the task and the resampling instance

library(mlr)
library(OpenML)

source("SO/probdesign.R")

path = "data/"

for (i in 1:length(datasets)) {
  OMLtask = convertOMLTaskToMlr(getOMLTask(datasets[[i]]))
  task = OMLtask$mlr.task
  rin = OMLtask$mlr.rin
  task.id = task$task.desc$id
  dir.create(file.path(path, task.id))
  saveRDS(task, paste(path, task.id, "task.rds", sep = "/"))
  saveRDS(rin, paste(path, task.id, "rin.rds", sep = "/"))
}
