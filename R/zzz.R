#' @import BBmisc
#' @import magrittr
#' @import checkmate
#' @import ParamHelpers
#' @import mlr
#' @import mlrCPO
#' @import mlrMBO
#' @import stats
#' @import mosmafs
#' @import kergp
#' @importFrom ecr computeHV
NULL

.onAttach = function(libname, pkgname) {
  parallelMap::parallelRegisterLevels(package = "mobafeas", levels = "gprestart")
}
