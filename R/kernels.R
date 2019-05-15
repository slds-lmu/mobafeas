
#' @title Feature Selection Kernels
#'
#' @description
#' Kernels defined on the space of possible feature selections. These should be run, possibly with
#' arguments, and the result given to `constructMBFLearner()`.
#'
#' @param limit.par `[logical(1)]` Whether to limit the parameter value range
#' @param data `[matrix | data.frame | Task]` the data to use.
#' @param allequal `[logical(1)]` Whether to generate one parameter per feature, or only one parameter
#'   overall.
#' @name Kernel
#' @aliases kernelMBFHamming kernelMBFGraph kernelMBFAgreement kernelMBFAgreeCor
#' @export
kernelMBFHamming <- function() function(d) {
  covMan(function(f1, f2, par) {
    sa <- -sum(abs(f1 - f2))
    K <- exp(sa * par)
    attr(K, "gradient") <- sa * K
    K
  }, TRUE, d = d,
  parLower = 1e-6,
  parUpper = -log(1e-8 / d),
  par = 1 / d,
  parNames = "theta")
}

#' @rdname Kernel
#' @export
kernelMBFGraph <- function(allequal = TRUE) function(d) {
  repno <- if (allequal) 1 else d
  parnames <- if (allequal) "beta" else sprintf("beta%s", seq_len(d))
  covMan(function(f1, f2, par) {
    K <- prod(ifelse(f1 == f2, (exp(-2 * par) + 1) / 2, -expm1(-2 * par) / 2))
    gradmul <- ifelse(f1 == f2, -2 / (1 + exp(2 * par)), 2 / expm1(2 * par))
    if (length(par) == 1) gradmul <- sum(gradmul)
    attr(K, "gradient") <- K * gradmul
    K
  }, TRUE, d = d,
  parLower = rep(0, repno),
  parUpper = rep(-log(1e-8 / d), repno),
  par = rep(1/d, repno),
  parNames = parnames)
}

#' @rdname Kernel
#' @export
kernelMBFAgreement <- function(limit.par = FALSE) function(d) {
  covMan(function(f1, f2, par) {
    disagreement <- mean(abs(f1 - f2))
    K <- 1 - disagreement * par
    attr(K, "gradient") <- -disagreement
    K
  }, TRUE, d = d,
  parLower = 0,
  parUpper = if (limit.par) 1 else Inf,
  par = 1,
  parNames = "theta")
}

#' @rdname Kernel
#' @export
kernelMBFAgreeCor <- function(data, limit.par = FALSE) function(d) {
  assert(
    checkMatrix(data),
    checkDataFrame(data),
    checkClass(data, "Task")
  )
  if (inherits(data, "Task")) {
    data <- getTaskData(data, target.extra = TRUE)$data
  }
  cormat <- abs(cor(data))
  assertMatrix(cormat, any.missing = FALSE)
  covMan(function(f1, f2, par) {
    subcor <- cormat[as.logical(f1), as.logical(f2)]
    disagreement <- (sum(1 - apply(subcor, 1, max)) + sum(1 - apply(subcor, 2, max))) / d
    K <- 1 - disagreement * par
    attr(K, "gradient") <- -disagreement
    K
  }, TRUE, d = d,
  parLower = 0,
  parUpper = if (limit.par) 1 else Inf,
  par = 1,
  parNames = "theta")
}
