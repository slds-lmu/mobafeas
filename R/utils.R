
makeConfigObject <- function(cl) {
  addClasses(as.list(parent.frame()), c(cl, "ConfigObject"))
}

print.ConfigObject <- function(x, ...) {
  catf("%s(%s)", class(x)[1], paste(sprintf("%s = %s", names(x), lapply(x, convertToShortString)), collapse = ", "))
}
