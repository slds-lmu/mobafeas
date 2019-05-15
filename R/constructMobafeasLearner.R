

#' @title Construct MoBaFeaS Gaussian Process Learner
#'
#' @description
#' The mobafeas Learner is specifically set up to handle feature configuration vectors
#' with a separate kernel.
#'
#' @param par.set `[ParamSet]` The parameter set of the objective function being optimized
#'   (probably the return value of `makeMobafeasObjective()`).
#' @param kernelconstructor `[function | NULL]` function `d -> kernel`, see the [`Kernel`] functions.
#' @param default.kernel `[character(1)]` One of `"matern3_2"` (default), `"matern5_2"`, `"exp"`, `"gauss"`.
#' @return `Learner`.
#' @family Surrogate Models
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
#' @description
#' Constructs the `randomForest` `Learner` that would be used by `mlrMBO` if the decision space
#' has discrete components.
#'
#' @family Surrogate Models
#' @export
constructRFSurrogate <- function() {
  lrn = makeLearner("regr.randomForest", se.method = "jackknife", keep.inbag = TRUE, predict.type = "se") %>%
    makeImputeWrapper(classes = list(numeric = imputeMax(2), factor = imputeConstant("__miss__")))
}
