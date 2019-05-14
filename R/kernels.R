

#' @export
kernelMBFHamming <- function(d) {
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

