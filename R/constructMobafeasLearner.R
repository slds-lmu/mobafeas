

#' @param kernelconstructor `[function | NULL]` function d -> kernel
#'
#' @export
constructMBFLearner <- function(par.set, kernelconstructor = NULL, default.kernel = "matern3_2") {
  selectorfeatures <- getParamIds(ps.objective$pars$selector.selection, repeated = TRUE, with.nr = TRUE)
  makeLearner("regr.kernel.gp", special.kernel = if (!is.null(kernelconstructor)) kernelconstructor(length(selectorfeatures)),
    special.kernel.features = if (is.null(kernelconstructor)) character(0) else selectorfeatures,
    default.kernel = default.kernel, predict.type = "se",
    print.level = 0)
}

#' @title Construct the default Random Forest surrogate learner for mlrMBO
#'
#' @export
constructRFSurrogate <- function() {
  lrn = makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE, predict.type = "se") %>%
    makeImputeWrapper(classes = list(numeric = imputeMax(2), factor = imputeConstant("__miss__")))
}
