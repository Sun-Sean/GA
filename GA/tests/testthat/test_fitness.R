# source("../../R/Fitness.R")

# create dummy data
x <- matrix(rnorm(100),10,10)
y <- rnorm(10)
P = 5
population = matrix(rep(c(1,0),25),5,10)
type = 'lm'

# run the fitness evaluation
result <- fitness_aic(x,y,population,P,type)


testthat::test_that("fitness function returns expected value", {
  expect_true(is.numeric(result))
  expect_length(result, P)
})
