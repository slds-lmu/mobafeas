# problem design
datafolder = "data"

# --- problem design ---
# datasets = list("sonar" = 39, "ionosphere" = 57, "madelon" = 145853,
# 	"hill-valley" = 9893, "wdbc" = 145878, "tecator" = 3716,
# 	"lsvt" = 9975, "clean1" = 146193)
datasets = list("sonar" = 39)#(, usps, cnae-9)

# --- specify learners ---
# Machine learning algorithms to be benchmarked
LEARNERS = list("SVM" = makeLearner("classif.ksvm", kernel = "rbfdot"),
	"kknn" = makeLearner("classif.kknn"),
	"xgboost" = makeLearner("classif.xgboost", id = "classif.xgboost", eval_metric = "error", objective = "binary:logistic")
	)
# 
# Tuning parameter sets to be benchmarked
# TODO: We will tune over numerical parameters only
# 		check if kknn can be done
PAR.SETS = list(
	SVM = makeParamSet(
		makeNumericParam("C", lower = -10, 10, trafo = function(x) 2^x),
		makeNumericParam("sigma", lower = -10, 10, trafo = function(x) 2^x)
	),
	kknn = makeParamSet(
		makeIntegerParam("k", lower = 1L, upper = 50L),
		makeNumericParam("distance", lower = 1, upper = 100),
		makeDiscreteParam("kernel", values = c("rectangular", "optimal", "triangular", "biweight"))
	),
	xgboost = makeParamSet(
	  	makeIntegerParam("nrounds", lower = 1L, upper = 5000L),	
	  	makeNumericParam("eta", lower = 0.01, upper = 0.2),
	  	makeNumericParam("gamma", lower = -7, upper = 6, trafo = function(x) 2^x),
	  	makeIntegerParam("max_depth", lower = 3, upper = 20),
	  	makeNumericParam("colsample_bytree", lower = 0.5, upper = 1),
	  	makeNumericParam("colsample_bylevel", lower = 0.5, upper = 1),
	  	makeNumericParam("lambda", lower = -10, upper = 10, trafo = function(x) 2^x),
	  	makeNumericParam("alpha", lower = -10, upper = 10, trafo = function(x) 2^x),
	  	makeNumericParam("subsample", lower = 0.5, upper = 1)
	)
)


OBJECTIVES = list(
		"SO" = FALSE,
		"MO" = TRUE,
		"scalar" = function(perf, featfrac) perf + featfrac
	)