#source("../../R/selection.R")

fitness = c(0.1, 0.03, 0.12, 0.02, 0.16, 0.19, 0.17, 0.05, 0.01, 0.15)
fitness_rank = rank(fitness)
K = 3

oneprop = onepropselection(fitness)
twoprop = twopropselection(fitness)
tou = tournament(fitness, K)
ranksl = rankselection(fitness_rank)
stochastic = sus(fitness)


testthat::test_that("seletion functions returns expected value", {
  expect_true(is.matrix(oneprop))
  expect_true(is.matrix(twoprop))
  expect_true(is.matrix(tou))
  expect_true(is.matrix(ranksl))
  expect_true(is.matrix(stochastic))
  
  expect_equal(dim(oneprop),c(length(fitness), 2))
  expect_equal(dim(twoprop), c(length(fitness), 2))
  expect_equal(dim(tou), c(length(fitness), 2))
  expect_equal(dim(ranksl), c(length(fitness), 2))
  expect_equal(dim(stochastic), c(length(fitness), 2))
})