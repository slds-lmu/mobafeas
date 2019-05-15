# infill crits take values, model predictions, model SEs, nugget info, residual variance info
# this is then integrated over the pareto fron
# (i.e. EI with points with same FF + (EI with points with same FF + points with one more feature) + (EI with points with same FF + pts with one or two more features))


#' @title Acquisition Function
#'
#' Evaluates the infill criterion if cached information about the population is present.
#'
#' If this is not the case, the criterion can be converted to an `MBOInfillCrit` using
#' `as.MBOInfillCrit()` and then executed directly.
#'
#' @param infobj `[Infill]` The acquisition function object
#' @param suggestions `[data.frame]` points to evaluate, with columns `"response"`, `"se"`, `"c2.value"`
#' @param population.info `[list]` with elements:\\
#'   - `$nadir` `[numeric(2)]` nadir point.\\
#'   - `paretofront` `[matrix]` pareto front of values seen so far: one column per individuum, two rows\\
#'     where the first row is the modeled one and the second row is the known one. Columns should be ordered
#'     by second row increasing.
#'   - `$pointdata` `[data.frame]` points known so far, with columns value, response, se, c2.value\\
#'   - `$nugget` `[numeric(1)]` model nugget
#' @return `[numeric]` vector of length `nrow(suggestions)` indicating desirability of each point. Larger values are better.
#' @family Infill Criteria
#' @export
acquisition <- function(infobj, suggestions, population.info) {
  UseMethod("acquisition")
}

#' @export
acquisition.default <- function(infobj, ...) {
  stopf("Object of class %s is not an acquisition function object", class(infobj))
}

#' @title Infill Criterion
#'
#' Different Infill criteria. Supported are:
#'
#' * Confidence Bound (`InfillCB`)
#' * Expected Improvement (`InfillEI`)
#' * Response (`InfillResponse`)
#'
#' @param lambda `[numeric(1)]`
#' @family Infill Criteria
#' @name Infill
#' @aliases InfillCB InfillEI InfillResponse
#' @export
InfillCB <- function(lambda = 2) {
  assertNumber(lambda)
  makeConfigObject(c("InfillCB", "Infill"))
}

#' @rdname Infill
#' @export
InfillEI <- function() {
  makeConfigObject(c("InfillEI", "Infill"))
}

#' @rdname Infill
#' @export
InfillResponse <- function() {
  makeConfigObject(c("InfillResponse", "Infill"))
}

#' @title Conver to MBOInfillCrit
#'
#' Necessary to use `mlrMBO::setMBOControlInfill()`.
#'
#' @param x [`Infill`] the Infill object to convert
#' @return `MBOInfillCrit`
#' @family Utility Functions
#' @family Infill Criteria
#' @export
as.MBOInfillCrit <- function(x) {
  UseMethod("as.MBOInfillCrit")
}

#' @export
as.MBOInfillCrit.Infill <- function(x) {
  fun <- function(points, models, control, par.set, designs, iter, progress, attributes = FALSE) {
    population.info <- computePopulationInfo(models[[1]], designs[[1]], control)
    suggestions <- predict(object = models[[1]], newdata = points)$data[c("response", "se")]
    suggestions$c2.value <- control$co.objective(points)

    -acquisition(x, suggestions, population.info)
  }
  attr(fun, "infill.crit.object") <- x
  addClasses(list(fun = fun, name = class(x)[1], id = class(x)[1], opt.direction = "maximize",
    components = character(0), params = list(), requires.se = TRUE), c(paste0("InfillCrit", toupper(class(x)[1])), "MBOInfillCrit"))
}

#' @export
acquisition.InfillCB <- function(infobj, suggestions, population.info) {
  suggestions$response <- suggestions$response - suggestions$se * infobj$lambda
  acquisition.InfillResponse(infobj, suggestions, population.info)
}

#' @export
acquisition.InfillEI <- function(infobj, suggestions, population.info) {
  integrand <- function(sug, upper) {
    d <- upper - sug$response
    d.scaled <- ifelse(d == 0, 0, d / sug$se)
    d * pnorm(d.scaled) + sug$se * dnorm(d.scaled)
  }
  integratePareto(integrand, suggestions, population.info)
}

#' @export
acquisition.InfillResponse <- function(infobj, suggestions, population.info) {
  integrand <- function(sug, upper) pmax(0, upper - sug$response)
  integratePareto(integrand, suggestions, population.info)
}

# fnc: function(suggestions, population.info)
integratePareto <- function(fnc, suggestions, population.info) {

  paretofront <- cbind(population.info$paretofront, c(0, population.info$nadir[2]))
  c2.lower <- 0
  c1.upper <- population.info$nadir[1]
  integral <- numeric(nrow(suggestions))
  for (pf in seq_len(ncol(paretofront))) {
    c2.upper <- paretofront[2, pf]
    addendum <- fnc(suggestions, c1.upper)
    mul <- pmax(0, c2.upper - pmax(c2.lower, suggestions$c2.value))
    integral <- integral + ifelse(mul == 0, 0, addendum * mul)
    c2.lower <- c2.upper
    c1.upper <- paretofront[1, pf]
  }
  integral
}

#' @export
acquisition.InfillEQI <- function(infobj, suggestions, population.info) {
  stop("Not yet implemented.")
}

#' @export
acquisition.InfillAEI <- function(infobj, suggestions, population.info) {
  stop("Not yet implemented.")
}
