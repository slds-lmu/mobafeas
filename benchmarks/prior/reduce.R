library(batchtools)
library(dplyr)
library(mlr)
library(mlrCPO)

source("probdesign.R")

# Reduce results
reg = loadRegistry("registry", writeable = FALSE)
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "learner", "cv.iters", "maxeval"))
tab = ijoin(tab, findDone())[job.id %in% 49:72, ]

path = "results_raw"
dir.create(path)

res = reduceResultsList(tab)
saveRDS(res, file.path(path, "results.rds"))
saveRDS(tab, file.path(path, "tab.rds"))

# Analyse results
library(ggplot2)
library(GGally)
library(parcoords)
library(plotly)

res = readRDS(file.path(path, "results.rds"))
tab = readRDS(file.path(path, "tab.rds"))

dir.create("results")

logtransformed = c("distance", "C", "sigma", "gamma", "lambda", "alpha")

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (lrn in c("SVM", "xgboost", "kknn")) {
	reslist = list()

	dir.create(file.path("results", lrn))

	# let us take the median of all runs for each model
	for (data in unique(tab$problem)) {
		idx = which(tab$problem == data & tab$learner == lrn)

		if (length(idx) > 0) {
				res.red = res[idx]
				bmr = res.red[[1]]$result$bmr
				r = getBMRTuneResults(bmr)[[1]][[1]]
		
				reslist[[data]] = do.call(rbind, lapply(r, function(s) as.data.frame(s$x)))

				get_optimal_param = function(x) {
					if (is.factor(x)) {
						z = getmode(x)
					} else {
					
					if (is.integer(x)) {
						z = round(median(x))
					} else {
						z = median(x)						
						}
					}
					return(z)
				}

				optimal.param = lapply(1:ncol(reslist[[data]]), function(x) get_optimal_param(reslist[[data]][, x]))

				saveRDS(optimal_param, paste("results/", lrn, "/", data, ".rds", sep = ""))

				for (col in 1:ncol(reslist[[data]])) {
					if (is.numeric(reslist[[data]][, col]) & !is.integer(reslist[[data]][, col])) {
						reslist[[data]][, col] = log(as.numeric(reslist[[data]][, col]), base = 2)
						colnames(reslist[[data]])[col] = paste("log_", colnames(reslist[[data]])[col], sep = "")
					}
				}
		
				reslist[[data]]$prob = data
			}
	}

	resr = do.call("rbind", reslist)
	resr$prob = as.factor(resr$prob)

	p = ggparcoord(data = resr, columns = c(1:(ncol(resr) - 1)), groupColumn = "prob") + ylab(c(""))

	# p = parcoords::parcoords(resr , rownames = FALSE,
 	#                     reorder = TRUE, brushMode = "1D")
	ggsave(paste("results/", lrn, "coordplot.png", sep = ""), p)

	write.csv2(summary(resr), paste("results/", lrn, "_summary.csv", sep = ""))

	write.csv2(cor(as.matrix(resr[, 1:(ncol(resr) - 1)])), paste("results/", lrn, "_cor.csv", sep = ""))

}

