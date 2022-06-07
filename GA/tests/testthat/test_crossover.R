library(assertthat)
library(testthat)
#source("crossover.R")

population <- sample(100:200, 1)
n <- sample(10:20, 1)
chromosomes <- matrix(rbinom(population*n,1, 0.5), population, n)
pairs <- matrix(sample(1:population, 2*population, replace = TRUE), population, 2)
offsprings <- crossover(pairs, chromosomes)

testthat::test_that("crossover() returns expected value", {
          expect_true(nrow(offsprings) == population)
          expect_true(ncol(offsprings) == n)
          expect_true(is.matrix(offsprings))
  })
