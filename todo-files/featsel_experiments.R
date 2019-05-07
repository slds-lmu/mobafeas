


data <- readRDS("../mosmafs-benchmark/benchmark/data/sonar/task.rds")

library("mlr")
library("parallelMap")

parallelStartMulticore(32)



cv310 <- makeResampleDesc("RepCV", reps = 10, folds = 10, stratify = TRUE)

lrns <- list(makeLearner("classif.randomForest"), makeLearner("classif.kknn"), makeLearner("classif.cvglmnet"))



ncols <- getTaskNFeats(data)

bitvecs <- lapply(seq(0, ncols), function(n) {
  (seq_len(ncols) <= n)[sample(ncols)]
})

bitvecs2 <- replicate(61, (seq_len(ncols) <= 4)[sample(ncols)], simplify = FALSE)

bvlister <- function(bitvecs) {
  bvcol <- list()
  for (b in bitvecs) {
    bd <- digest::digest(b)
    bvcol[[bd]] <- b
    for (col in seq_len(ncols)) {
      bcopy <- b
      bcopy[col] <- !bcopy[col]
      bd <- digest::digest(bcopy)
      bvcol[[bd]] <- bcopy
    }
  }
  bvcol
}

crossbvlister <- function(bitvecs) {
  bvcol <- list()
  for (b in bitvecs) {
    bd <- digest::digest(b)
    bvcol[[bd]] <- b
    for (col in seq_len(ncols)) {
      for (col2 in seq_len(ncols)) {
        bcopy <- b
        bcopy[col] <- !bcopy[col]
        bd <- digest::digest(bcopy)
        bvcol[[bd]] <- bcopy

        bcopy <- b
        bcopy[col2] <- !bcopy[col2]
        bd <- digest::digest(bcopy)
        bvcol[[bd]] <- bcopy

        bcopy <- b
        bcopy[col] <- !bcopy[col]
        bcopy[col2] <- !bcopy[col2]
        bd <- digest::digest(bcopy)
        bvcol[[bd]] <- bcopy
      }
    }
  }
  bvcol
}

bvcol <- bvlister(bitvecs)
bvcol2 <- bvlister(bitvecs2)

bvcolcross <- crossbvlister(bitvecs)
bvcolcross2 <- crossbvlister(bitvecs2)
# saveRDS(bitvecs, "bitvecs.rds")
# saveRDS(bitvecs2, "bitvecs2.rds")
# saveRDS(bvcol, "bvcol.rds")
# saveRDS(bvcol2, "bvcol2.rds")
# saveRDS(bvcolcross, "bvcolcross.rds")
# saveRDS(bvcolcross2, "bvcolcross2.rds")

length(bvcolcross)

bitvecs <- readRDS("bitvecs.rds")
bitvecs2 <- readRDS("bitvecs2.rds")
bvcol <- readRDS("bvcol.rds")
bvcol2 <- readRDS("bvcol2.rds")
bvcolcross <- readRDS("bvcolcross.rds")
bvcolcross2 <- readRDS("bvcolcross2.rds")

## configureMlr(show.info = FALSE, on.learner.error="quiet")
## parallelStop()

## bvres <- parallel::mclapply(bvcol, function(b) {
##   resample(lrns[[3]], subsetTask(data, features = which(b)), hout)$aggr
## }, mc.cores = 32)

## resample(lrns[[3]], data, cv310)


allresults <- readRDS("result_ALL.rds")
allresults.x <- readRDS("result_ALL_supp.rds")

allresults.cf <- readRDS("result_ALL_2.rds")

allresults.cross <- readRDS("result_cross_all.rds")
allresults.cross2 <- readRDS("result_cross2_all.rds")

meanresults <- function(allresults) t(sapply(names(allresults[[1]][[1]]), function(n) {
  # allresults is list with indices [replicate][learner][bitvec digest]
  sapply(1:3, function(i) {
    mean(sapply(allresults, function(a) a[[i]][[n]]))
  })
}))

meanresults.slices <- function(allresults) meanresults(parallel::mclapply(allresults, function(replicate) {
  # allresults is list with indices [replicate][slice][learner][bitvec digest]
  lapply(seq_along(replicate[[1]]), function(lrnidx) {
    unlist(lapply(replicate, function(slice) {
      slice[[lrnidx]]
    }), recursive = FALSE)
  })
}, mc.cores = 16))

resultmeaned <- meanresults(c(allresults, allresults.x))
resultmeaned.cf <- meanresults(allresults.cf)

resultmeaned.cross <- meanresults.slices(allresults.cross)
resultmeaned.cross2 <- meanresults.slices(allresults.cross2)
# saveRDS(resultmeaned.cross, "resultmeaned.cross.rds")
# saveRDS(resultmeaned.cross2, "resultmeaned.cross2.rds")
resultmeaned.cross2 <- readRDS("resultmeaned.cross2.rds")
resultmeaned.cross <- readRDS("resultmeaned.cross.rds")

getperfarray <- function(bitvecs, resultmeaned) sapply(bitvecs, function(bv) {
  sapply(seq_len(ncols), function(col) {
    bcopy <- bv
    bcopy[col] <- !bcopy[col]
    if (bcopy[col]) {
      has <- digest::digest(bcopy)
      hasnot <- digest::digest(bv)
    } else {
      hasnot <- digest::digest(bcopy)
      has <- digest::digest(bv)
    }
    resultmeaned[has, ] - resultmeaned[hasnot, ]
  }, simplify = "array")
}, simplify = "array")


# bitvecs: the initial bit vectors
# resultmeaned: created above
# otherstate: whether the 2nd bit position should be 1 or 0
getperfarray2d <- function(bitvecs, resultmeaned, otherstate) {
  x <- parallel::mclapply(bitvecs, function(bv.outer) {
    otherstate = !(!(otherstate))
    sapply(seq_len(ncols), function(col1)
      sapply(seq_len(ncols), function(col2) {
        bv <- bv.outer
        bv[col2] <- otherstate
        bcopy <- bv
        bcopy[col1] <- !bcopy[col1]
        if (bcopy[col1]) {
          has <- digest::digest(bcopy)
          hasnot <- digest::digest(bv)
        } else {
          hasnot <- digest::digest(bcopy)
          has <- digest::digest(bv)
        }
        resultmeaned[has, ] - resultmeaned[hasnot, ]
      }, simplify = "array")
    , simplify = "array")
  }, mc.cores = 32)
  names(x) <- names(bitvecs)
  simplify2array(x)
}

perfarray <- getperfarray(bitvecs, resultmeaned)
perfarray.cf <- getperfarray(bitvecs2, resultmeaned.cf)


perfarray.cross.plus <- getperfarray2d(bitvecs, resultmeaned.cross, TRUE)
perfarray.cross.minus <- getperfarray2d(bitvecs, resultmeaned.cross, FALSE)
perfarray.cross2.plus <- getperfarray2d(bitvecs2, resultmeaned.cross2, TRUE)
perfarray.cross2.minus <- getperfarray2d(bitvecs2, resultmeaned.cross2, FALSE)

saveRDS(list(
    perfarray.cross.plus = perfarray.cross.plus,
    perfarray.cross.minus = perfarray.cross.minus,
    perfarray.cross2.plus = perfarray.cross2.plus,
    perfarray.cross2.minus = perfarray.cross2.minus,
    bitvecs = bitvecs,
    bitvecs2 = bitvecs2),
  "perfarray.cross.rds", compress = "xz")

str(perfarray.cross.plus)

image(apply(perfarray.cross2.plus, c(2, 3), sd, na.rm = TRUE))
image(apply(perfarray.cross2.minus, c(2, 3), sd, na.rm = TRUE))

image(-apply(perfarray.cross2.plus, c(2, 3), mean, na.rm = TRUE))
image(-apply(perfarray.cross2.minus, c(2, 3), mean, na.rm = TRUE))

image(apply(perfarray.cross.plus, c(2, 3), sd, na.rm = TRUE))
image(apply(perfarray.cross.minus, c(2, 3), sd, na.rm = TRUE))

image(-apply(perfarray.cross.plus, c(2, 3), mean, na.rm = TRUE))
image(-apply(perfarray.cross.minus, c(2, 3), mean, na.rm = TRUE))


heatmap

# perfarray: 3 learners x 60 feature inclusion diffs x
# 61 feature fraction baselines (in perfarray) or just 61 retries (in perfarray.cf)

# matplot: x axis goes along rows
# --> this is a plot along which column of the dataset is added / removed
matplot(apply(perfarray, c(2, 3), mean))

matplot(apply(perfarray.cf, c(2, 3), mean))

plot(apply(perfarray, 2, mean, na.rm = TRUE), apply(perfarray.cf, 2, mean, na.rm = TRUE))


plot(apply(apply(perfarray, c(2, 3), mean), 1, sd, na.rm = TRUE))

for (prazniks in c("CMIM", "MIM", "JMIM", "NJMIM", "JMI", "MRMR", "DISR")) {
  dx <- generateFilterValuesData(data, sprintf("praznik_%s", prazniks))$data[[3]]
  cat(sprintf("%s: %s\n", prazniks, cor(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean), dx, method = "spearman")))
}

for (prazniks in c("CMIM", "MIM", "JMIM", "NJMIM", "JMI", "MRMR", "DISR")) {
  dx <- generateFilterValuesData(data, sprintf("praznik_%s", prazniks))$data[[3]]
  cat(sprintf("%s: %s\n", prazniks, cor(apply(apply(perfarray.cf, c(1, 2), sd, na.rm = TRUE), 2, mean), dx, method = "spearman")))
}


dfs <- getTaskData(data, target.extra = TRUE)


plot(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean), praznik::miScores(dfs$data, dfs$target), xlim = c(0, 0.1), ylim = c(0, 0.2))
points(apply(apply(perfarray.cf, c(1, 2), sd, na.rm = TRUE), 2, mean), praznik::miScores(dfs$data, dfs$target), pch = "x")
# perfarray.cf is shifted to the right, i.e. larger s.d. depending on background features selected
mat <- cbind(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean), praznik::miScores(dfs$data, dfs$target))
px <- prcomp(mat)$rotation
beta <- px[2, 1] / px[1, 1]
icpt <- mean(mat[, 2] - beta * mat[, 1])
abline(icpt, beta)


plot(-apply(apply(perfarray, c(1, 2), mean, na.rm = TRUE), 2, mean), praznik::miScores(dfs$data, dfs$target), xlim = c(0, 0.1), ylim = c(0, 0.2))
plot(-apply(apply(perfarray.cf, c(1, 2), mean, na.rm = TRUE), 2, mean), praznik::miScores(dfs$data, dfs$target), xlim = c(0, 0.1), ylim = c(0, 0.2))

plot(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean), apply(apply(perfarray, c(1, 2), mean, na.rm = TRUE), 2, mean), xlim = c(0, 0.05), ylim = c(-0.05, 0.01))
abline(.01, -1)
points(apply(apply(perfarray.cf, c(1, 2), sd, na.rm = TRUE), 2, mean), apply(apply(perfarray.cf, c(1, 2), mean, na.rm = TRUE), 2, mean), pch = "x")
abline(.03, -2)



# columns ordered by praznik output
ord <- order(generateFilterValuesData(data, "praznik_CMIM")$data[[3]])
matplot(apply(perfarray, c(2, 3), mean)[ord, ])
plot(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean)[ord])
cor(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean), ord, method = "spearman")

ord <- order(generateFilterValuesData(data, "praznik_MIM")$data[[3]])
matplot(apply(perfarray, c(2, 3), mean)[ord, ])
plot(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean)[ord])
cor(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean), ord, method = "spearman")

ord <- order(generateFilterValuesData(data, "praznik_JMIM")$data[[3]])
matplot(apply(perfarray, c(2, 3), mean)[ord, ])
plot(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean)[ord])
cor(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean), ord, method = "spearman")

ord <- order(generateFilterValuesData(data, "praznik_NJMIM")$data[[3]])
matplot(apply(perfarray, c(2, 3), mean)[ord, ])
plot(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean)[ord])
cor(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean), ord, method = "spearman")

ord <- order(generateFilterValuesData(data, "praznik_JMI")$data[[3]])
matplot(apply(perfarray, c(2, 3), mean)[ord, ])
plot(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean)[ord])
cor(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean), ord, method = "spearman")

ord <- order(generateFilterValuesData(data, "praznik_MRMR")$data[[3]])
matplot(apply(perfarray, c(2, 3), mean)[ord, ])
plot(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean)[ord])
cor(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean), ord, method = "spearman")

ord <- order(generateFilterValuesData(data, "praznik_DISR")$data[[3]])
matplot(apply(perfarray, c(2, 3), mean)[ord, ])
plot(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean)[ord])
cor(apply(apply(perfarray, c(1, 2), sd, na.rm = TRUE), 2, mean), ord, method = "spearman")



# --> this is a plot along how many columns of the dataset are present
matplot(t(apply(perfarray, c(2, 3), mean)))
plot(apply(t(apply(perfarray, c(2, 3), mean)), 1, sd, na.rm = TRUE))

plot(apply(t(apply(perfarray, c(2, 3), mean)), 1, mean, na.rm = TRUE))


mean(perfarray, na.rm = TRUE)

abline(0, 0)
