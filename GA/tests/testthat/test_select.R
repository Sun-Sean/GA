#source("../../R/select.R")
#source("../../R/crossover.R")
#source("../../R/Fitness.R")
#source("../../R/Initialization.R")
#source("../../R/Mutation.R")
#source("../../R/selection.R")

# import library to create toy data for glm example
library("VSURF")
data("toys")
set.seed(1)
x <- toys[['x']]
y <- toys[['y']]

# selection results
result_glm <- select(x,y,p=10,selection_type = 'tournament',max_iter = 20,model_type = 'glm',k=5,threshold = 10^-3 )

# create data for lm example
set.seed(5)
x <- rnorm(1000,3,2)
dim(x) <- c(100,10)
y <- 3*x[,1] + 4*x[,7]- 5*x[,9] + rnorm(100)

# selection results
result_lm <- select(x,y,p=10,selection_type = 'tournament',max_iter = 100,k=5,threshold = 10^-4 )




testthat::test_that("select function returns expected value", {
  expect_true(is.list(result_glm))
  expect_true(is.list(result_lm))
  
  expect_true(is.number(result_glm[[1]]))
  expect_true(is.number(result_lm[[1]]))
  
  expect_true(is.numeric(result_glm[[2]]))
  expect_true(is.numeric(result_lm[[2]]))
  
  expect_length(result_glm[[2]],200)
  expect_length(result_lm[[2]],10)
})
