library(batchtools)
library(dplyr)
library(mlr)
library(mlrCPO)

source("probdesign.R")

# Reduce results
reg = loadRegistry("registry", writeable = FALSE)
tab = summarizeExperiments(by = c("job.id", "algorithm", "problem", "learner"))

path = "results_raw"
dir.create(path)

res = reduceResultsList(tab)
saveRDS(res, file.path(path, "results.rds"))
saveRDS(tab, file.path(path, "tab.rds"))

# Analyse results
library(ggplot2)
library(GGally)
library(parcoords)

res = readRDS(file.path(path, "results.rds"))
tab = readRDS(file.path(path, "tab.rds"))

dir.create("results")

for (lrn in c("SVM", "xgboost", "kknn")) {
	reslist = list()

	dir.create(file.path("results", lrn))

	for (data in unique(tab$problem)) {
		idx = tab[problem == data & learner == lrn, ]$job.id

		res.red = res[idx]
		bmr = res.red[[1]]$result$bmr
		r = getBMRTuneResults(bmr)[[1]][[1]]

		reslist[[data]] = data.frame(t(sapply(r, function(s) unlist(s$x))))
		reslist[[data]]$prob = data
	}

	resr = do.call("rbind", reslist)
	resr$prob = as.factor(resr$prob)

	p = parcoords::parcoords(resr , rownames = FALSE,
                     reorder = TRUE, brushMode="1D")

	htmlwidgets::saveWidget(as_widget(p), paste(lrn, ".html", sep = ""))

	# ggsave(file.path("results", lrn, "coordplot.png"), p)
}

