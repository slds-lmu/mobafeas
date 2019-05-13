



collectMBFResult <- function(opt.state, aggregate.perresult = list(domHV = function(x) computeHV(x, ref.point)), ref.point = c(max(opt.state$opt.path$env$path$y), 1)) {
  normalize.funlist <- function(fl) {
    assertList(fl, any.missing = FALSE, types = c("function",
      "character"))
    charentries <- vlapply(fl, is.character)
    names(fl)[charentries] <- ifelse(is.na(names2(fl)[charentries]),
      unlist(fl[charentries], recursive = FALSE), names2(fl)[charentries])
    fl[charentries] <- lapply(fl[charentries], get, envir = .GlobalEnv,
      mode = "function")
    assertList(fl, any.missing = FALSE, types = "function",
      names = "unique")
  }
  aggregate.perresult <- normalize.funlist(aggregate.perresult)

  aggregate.fitness <- function(fitness) {
    resmat <- sapply(fitness, function(fit) {
      if (is.null(rownames(fit))) {
        rownames(fit) <- paste0("obj.", seq_len(nrow(fit)))
      }
      c(dummy = 0, vnapply(aggregate.perresult, function(f) f(fit)))
    })
    as.data.frame(t(resmat))[-1]
  }

  opt.path <- opt.state$opt.path
  ps <- getParamSet(opt.state$final.opt.state$opt.problem$fun)
  odf <- as.data.frame(opt.path, include.x = TRUE, include.y = FALSE, include.rest = FALSE)
  odf <- convertDataFrameCols(odf, ints.as.num = TRUE, logicals.as.factor = TRUE)
  individuals <- dfRowsToList(odf, ps)
  c2.value <- apply(odf[grepl("^selector\\.selection[0-9]+$", colnames(odf))], 1, mean)

  yval <- as.data.frame(opt.path, include.x = FALSE, include.y = TRUE, include.rest = FALSE)
  extras <- as.data.frame(opt.path, include.x = FALSE, include.y = FALSE, include.rest = TRUE)
  extras$eol <- NULL
  colnames(yval) <- "perf"

  holdout.col <- intersect("holdout", colnames(extras))
  has.holdout <- length(holdout.col) != 0

  resdf <- cbind(extras[c(grep("\\.time$", colnames(extras), value = TRUE), "dob", "prop.type", grep("^error\\.", colnames(extras), value = TRUE),
    getMBOInfillCritId(opt.state$control$infill.crit))],
    yval, propfeat = c2.value, extras[holdout.col])

  skiprows <- sum(extras$dob == 0)
  getfits <- function(which.col) {
    lapply(seq(skiprows + 1, nrow(resdf)), function(subset) {
      t(as.matrix(resdf[seq_len(subset), c(which.col, "propfeat")]))
    })
  }

  fitnesses <- getfits("perf")
  fillmat <- matrix(NA, nrow = skiprows, ncol = length(aggregate.perresult))
  fillvec <- rep(NA, skiprows)
  colnames(fillmat) <- names(aggregate.perresult)
  evalmat <- rbind(fillmat, aggregate.fitness(fitnesses))
  colnames(evalmat) <- sprintf("eval.%s", colnames(evalmat))
  resdf <- cbind(resdf, evalmat)

  if (has.holdout) {
    hofitnesses <- getfits("holdout")
    true.hout.domHV <- c(fillvec, mapply(function(eval.fit, hout.fit) {
      unbiasedHoldoutDomHV(eval.fit, hout.fit, ref.point)
    }, fitnesses, hofitnesses))
    naive.hout.domHV <- c(fillvec, mapply(function(eval.fit, hout.fit) {
      naiveHoldoutDomHV(eval.fit, hout.fit, ref.point)
    }, fitnesses, hofitnesses))
    houtmat <- rbind(fillmat, aggregate.fitness(hofitnesses))
    colnames(houtmat) <- sprintf("hout.%s", colnames(houtmat))
    resdf <- cbind(resdf, houtmat,
      true.hout.domHV, naive.hout.domHV)
  }

  resdf
}
