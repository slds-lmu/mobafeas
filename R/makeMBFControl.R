

#' @title MoBaFeaS Configuration Object
#'
#' @description
#' Configure the `mobafeasMBO()` run. This uses a `mlrMBO::makeMBOControl()` object and
#' sets it up for the specific needs of mobafeas. The resulting object should still have
#' an adequate terminator set using `mlrMBO::setMBOControlTermination()`.
#'
#' @param mosmafs.config `[MosmafsConfig]` An object created by `MosmafsConfig()`.
#' @param propose.points `[integer(1)]` How many points to propose per model fit. Currently
#'   only supports 1.
#' @param infill.crit `[Infill]` An [`Infill`] object to use.
#' @return `MBOControl`
#' @family Control Objects
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
#  ctrl$n.objectives <- 2
#  ctrl$y.name <- sprintf("y_%s", seq_len(ctrl$n.objectives))
#  ctrl$multiobj.method <- "semi-dib"
  ctrl
}

#' @title Change the Configuration Object Infill Optimization
#'
#' @description
#' Set the infill optimization config or infill criterion of the `makeMBFControl()`
#' generated control object.
#'
#' @param control `[MBOControl]` Control object created with `makeMBFControl()`.
#' @param infill.crit `[Infill]` An [`Infill`] object to use.
#' @param mosmafs.config `[MosmafsConfig]` An object created by `MosmafsConfig()`.
#' @return `MBOControl`
#' @family Control Objects
#' @export
setMBFControlInfill <- function(control, infill.crit = NULL, mosmafs.config = NULL) {
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

