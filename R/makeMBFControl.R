
#' @export
makeMBFControl <- function(mosmafs.config, propose.points = 1, infill.crit = InfillEI()) {
  # TODO: propose point method
  # TODO: give infill crit
  assertInt(propose.points, lower = 1)
  assertClass(infill.crit, "Infill")
  assertClass(mosmafs.config, "MosmafsConfig")
  if (propose.points != 1) {
    stop("Propose multiple points not yet supported")
  }
  ctrl <- makeMBOControl(n.objectives = 1, propose.points = propose.points, final.evals = 0, y.name = "y") %>%
    setMBOControlInfill() %>% setMBFControlInfill(infill.crit, mosmafs.config)
  ctrl$n.objectives <- 2
  ctrl$multiobj.method <- "semi-dib"
  ctrl
}

#' @export
setMBFControlInfill <- function(control, infill.crit, mosmafs.config = NULL) {
  assertClass(mosmafs.config, "MosmafsConfig", null.ok = TRUE)
  assertClass(infill.crit, "Infill", null.ok = TRUE)
  control$infill.opt <- "infillOptMobafeas"
  if (!is.null(infill.crit)) {
    control$infill.crit <- as.MBOInfillCrit(infill.crit)
  }
  if (!is.null(mosmafs.config)) {
    control$mosmafs <- mosmafs.config
  }
  control
}

# infill crits take values, model predictions, model SEs, nugget info, residual variance info
# this is then integrated over the pareto fron
# (i.e. EI with points with same FF + (EI with points with same FF + points with one more feature) + (EI with points with same FF + pts with one or two more features))


