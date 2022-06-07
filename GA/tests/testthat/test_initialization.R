# source("../../R/Initialization.R")

# create sample data
x <- (matrix(100,10,10))
p <- 5

# initialize
result <- init(p,x)


testthat::test_that("initialization returns expected value", {
  expect_true(is.matrix(result))
  expect_equal(dim(result),c(5,10))
})

