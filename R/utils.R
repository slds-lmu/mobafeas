
makeConfigObject <- function(cl) {
  addClasses(as.list(parent.frame()), c(cl, "ConfigObject"))
}

print.ConfigObject <- function(x, ...) {
  catf("%s(%s)", class(x)[1], paste(sprintf("%s = %s", names(x), lapply(x, convertToShortString)), collapse = ", "))
}

fixDesignLogicals <- function(df, ps) {
  coltypes <- getParamTypes(ps, df.cols = TRUE)
  psvals <- rep(getValues(ps)[getParamIds(ps)], getParamLengths(ps))
  for (col in seq_along(df)) {
    if (coltypes[col] == "logical") {
      df[[col]] <- as.logical(df[[col]])
    }
  }
  return(df)
}
