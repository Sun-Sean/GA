# source("../../R/Mutation.R")

# create matrix, and perform mutation
x <- matrix(rep(c(1,0),5),5,2)
# perform mutation
result <- mutation(mat=x,mu=0.5)

testthat::test_that("mutation function returns expected value", {
  expect_equal(dim(result), c(5,2))
  expect_true(is.matrix(result))
})
