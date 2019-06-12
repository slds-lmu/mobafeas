library(data.table)
library(ggplot2)
library(batchtools)
library(ecr)
library(gridExtra)
library(Hmisc)
library(dplyr)
library(plyr)

source("helpers.R")

# savepath
plotspath = "results_plots"
data_path = "results_raw"

# --- read the data ---
res = readRDS(paste(data_path, "/res.rds", sep = ""))
tab = res[, - 2]
job.ids = res$job.id
res.ob = res$result

reslist = lapply(1:length(job.ids), transform)
res = do.call(rbind, reslist)

res = ijoin(res[, - 2], tab, by = "job.id")

df = res[objective == "SO" , ]
df$job.id = as.factor(df$job.id)

# optimization progress
p = ggplot(data = df[dob > 0, ])
p = p + geom_line(aes(x = dob, y = perf.cumin, colour = kernel, group = job.id, lty = infill.crit))
p = p + facet_grid(vars(joint.hyperpars), vars(initialization))
p

p = ggplot(data = df[dob > 0, ])
p = p + geom_line(aes(x = dob, y = hout.cumin, colour = kernel, group = job.id, lty = infill.crit))
p = p + facet_grid(vars(joint.hyperpars), vars(initialization))
p

dfbla = df[, mean(holdout), by = c("dob", "objective", "infill.crit", "kernel", "initialization", "joint.hyperpars")]

p = ggplot(data = dfbla)
p = p + geom_line(aes(x = dob, y = V1, colour = kernel, lty = infill.crit))
p = p + facet_grid(vars(joint.hyperpars), vars(initialization))
p

df = res[objective == "MO" , ]
dfbla = df[, mean(true.hout.domHV), by = c("dob", "objective", "infill.crit", "kernel", "initialization", "joint.hyperpars")]

p = ggplot(data = dfbla)
p = p + geom_line(aes(x = dob, y = V1, colour = kernel, lty = infill.crit))
p = p + facet_grid(vars(initialization), vars(joint.hyperpars))
p


transform = function(i) {
	tf = cbind(job.id = job.ids[i], res.ob[[i]], infill.crit = names(res.ob[[i]])[8])
	names(tf)[9] = "infill_value"
	tf$perf.cumin = cummin(tf$perf)
	tf$hout.cumin = cummin(tf$holdout)
	return(tf)
	}