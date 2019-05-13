
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
  fun <- function(points, models, control, par.set, designs, iter, progress, attributes = FALSE) {
    # --- TODO: this is the same as in infillOpt.R
    model <- models[[1]]
    design <- designs[[1]]
    design.X <- design
    design.X[[control$y.name]] <- NULL
    pointdata <- predict(object = model, newdata = design.X)$data[c("response", "se")]
    pointdata$value <- design[[control$y.name]]
    pointdata$c2 <- apply(design.X[grepl("^selector\\.selection[0-9]+$", colnames(design.X))], 1, mean)
    popmatrix <- rbind(pointdata$value, pointdata$c2)
    paretofront <- popmatrix[, ecr::nondominated(popmatrix), drop = FALSE]
    paretofront <- paretofront[, order(paretofront[2, ]), drop = FALSE]
    nugget <- model$nugget # TODO
    # --- TODO end
    # newdesign <- fixDesignLogicals(args, par.set)  # if we treat logicals as factors, and I think this is what will happen, we don't need this.
    suggestions <- predict(object = model, newdata = points)$data[c("response", "se")]
    suggestions$c2.value <- apply(points[grepl("^selector\\.selection[0-9]+$", colnames(points))], 1, mean)

    -acquisition(x, suggestions, c(Inf, 1), paretofront, pointdata, nugget)
  }
  attr(fun, "infill.crit.object") <- x
  addClasses(list(fun = fun, name = class(x)[1], id = class(x)[1], opt.direction = "maximize",
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
    integral <- integral + ifelse(mul == 0, 0, addendum * mul)
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
