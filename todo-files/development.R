

library("mlr")
library("mlrCPO")
library("mlrMBO")
library("mosmafs")

roxygen2::roxygenise("..")

devtools::load_all("..")

x <- function() 1
y <- function() 2

mf <- function(name)  match.fun(name)

