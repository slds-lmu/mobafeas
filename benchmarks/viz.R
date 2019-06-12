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

transform = function(i) {
	tf = cbind(job.id = job.ids[i], res.ob[[i]], infill.crit = names(res.ob[[i]])[8])
	names(tf)[9] = "infill_value"
	tf$perf.cumin = cummin(tf$perf)
	tf$hout.cumin = cummin(tf$holdout)
	return(tf)
	}

# --- read the data ---
res = readRDS(paste(data_path, "/res.rds", sep = ""))
tab = res[, - 2]
job.ids = res$job.id
res.ob = res$result

reslist = lapply(1:length(job.ids), transform)
res = do.call(rbind, reslist)

res = ijoin(res[, - 2], tab, by = "job.id")

df = res[objective == "MO" & infill.crit == "InfillCB", ]
df$job.id = as.factor(df$job.id)

# optimization progress

dfbla = df[, mean(perf.cumin), by = c("dob", "objective", "infill.crit", "kernel", "initialization", "joint.hyperpars")]
p = ggplot(data = dfbla)
p = p + geom_line(aes(x = dob, y = V1, colour = kernel, lty = infill.crit))
p = p + facet_grid(vars(initialization), vars(joint.hyperpars))
p

dfbla = df[, mean(hout.cumin), by = c("dob", "objective", "infill.crit", "kernel", "initialization", "joint.hyperpars")]
p = ggplot(data = dfbla)
p = p + geom_line(aes(x = dob, y = V1, colour = kernel, lty = infill.crit))
p = p + facet_grid(vars(initialization), vars(joint.hyperpars))
p

dfbla = df[, mean(holdout), by = c("dob", "objective", "infill.crit", "kernel", "initialization", "joint.hyperpars")]

p = ggplot(data = dfbla[infill.crit == "InfillCB", ])
p = p + geom_line(aes(x = dob, y = V1, colour = kernel, lty = infill.crit))
p = p + facet_grid(vars(joint.hyperpars), vars(initialization))
p



df = res[objective == "MO" , ]
dfbla = df[, mean(true.hout.domHV), by = c("dob", "objective", "infill.crit", "kernel", "initialization", "joint.hyperpars")]

p = ggplot(data = dfbla[infill.crit == "InfillCB", ])
p = p + geom_line(aes(x = dob, y = V1, colour = kernel, lty = infill.crit))
p = p + facet_grid(vars(initialization), vars(joint.hyperpars))
p


