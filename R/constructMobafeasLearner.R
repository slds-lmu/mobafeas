

#' @param kernelconstructor `[function]` function d -> kernel
#' @export
constructMBFLearner <- function(par.set, kernelconstructor, default.kernel = "matern3_2") {
  selectorfeatures <- getParamIds(ps.objective$pars$selector.selection, repeated = TRUE, with.nr = TRUE)
  makeLearner("regr.kernel.gp", special.kernel = kernelconstructor(length(selectorfeatures)),
    special.kernel.features = selectorfeatures,
    default.kernel = default.kernel, predict.type = "se")
}
