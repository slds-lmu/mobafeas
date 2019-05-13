
#' @title Acquisition Function
#'
#' @param infobj [any] The acquisition function object
#' @param suggestions [`data.frame`] points to evaluate, with columns response, se, c2.value
#' @param nadir [`numeric(2)`] nadir point.
#' @param paretofront [`matrix`] pareto front of values seen so far: one column per individuum, two rows
#'   where the first row is the modeled one and the second row is the known one. Columns should be ordered
#'   by second row increasing.
#' @param pointdata [`data.frame`] points known so far, with columns value, response, se, c2.value
#' @param nugget [`numeric(1)`] model nugget
#' @return [`numeric`] vector of length `nrow(suggestions)` indicating desirability of each point. Larger values are better.
#' @export
acquisition <- function(infobj, suggestions, nadir, paretofront, pointdata, nugget) {
  UseMethod("acquisition")
}

acquisition.default <- function(infobj, ...) {
  stopf("Object of class %s is not an acquisition function object", class(infobj))
}

#' @title Confidence Bound Infill Criterion
#'
#' @param lambda [`numeric(1)`]
#' @family Infill Criteria
#' @export
InfillCB <- function(lambda = 2) {
  assertNumber(lambda)
  makeConfigObject(c("InfillCB", "Infill"))
}

#' @title Expected Improvement Infill Criterion
#'
#' @family Infill Criteria
#' @export
InfillEI <- function() {
  makeConfigObject(c("InfillEI", "Infill"))
}

#' @title Response Infill Criterion
#'
#' @family Infill Criteria
#' @export
InfillResponse <- function() {
  makeConfigObject(c("InfillResponse", "Infill"))
}

#' @title Conver to MBOInfillCrit
#'
#' Necessary to use `mlrMBO::setMBOControlInfill()`.
#'
#' @param x [`Infill`] the Infill object to convert
#' @export
as.MBOInfillCrit <- function(x) {
  UseMethod("as.MBOInfillCrit")
}

as.MBOInfillCrit.Infill <- function(x) {
  addClasses(list(fun = x, name = class(x)[1], id = class(x)[1], opt.direction = "maximize",
    components = character(0), params = list(), requires.se = TRUE), c(paste0("InfillCrit", toupper(class(x)[1])), "MBOInfillCrit"))
}


acquisition.InfillCB <- function(infobj, suggestions, nadir, paretofront, ...) {
  suggestions$response <- suggestions$response - suggestions$se * infobj$lambda
  acquisition.InfillResponse(infobj, suggestions, nadir, paretofront, ...)
}

acquisition.InfillEI <- function(infobj, suggestions, nadir, paretofront, ...) {
  integrand <- function(sug, upper) {
    d <- upper - sug$response
    d.scaled <- ifelse(d == 0, 0, d / sug$se)
    d * pnorm(d.scaled) + sug$se * dnorm(d.scaled)
  }
  integratePareto(integrand, suggestions, nadir, paretofront)
}

acquisition.InfillResponse <- function(infobj, suggestions, nadir, paretofront, ...) {
  integrand <- function(sug, upper) pmax(0, upper - sug$response)
  integratePareto(integrand, suggestions, nadir, paretofront)
}

# fnc: function(suggestions, upper)
integratePareto <- function(fnc, suggestions, nadir, paretofront) {
  paretofront <- cbind(paretofront, c(0, nadir[2]))
  c2.lower <- 0
  c1.upper <- nadir[1]
  integral <- numeric(nrow(suggestions))
  for (pf in seq_len(ncol(paretofront))) {
    c2.upper <- paretofront[2, pf]
    addendum <- fnc(suggestions, c1.upper)
    mul <- pmax(0, c2.upper - pmax(c2.lower, suggestions$c2.value))
    integral <- integral + addendum * mul
    c2.lower <- c2.upper
    c1.upper <- paretofront[1, pf]
  }
  integral
}

acquisition.InfillEQI <- function(infobj, suggestions, nadir, paretofront, pointdata, nugget) {
  stop("Not yet implemented.")
}

acquisition.InfillAEI <- function(infobj, suggestions, nadir, paretofront, pointdata, nugget) {
  stop("Not yet implemented.")
}