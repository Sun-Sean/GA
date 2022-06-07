#' Fitness
#' Calculate the fitness of each individual in the population, the AICs() function works similarly but return AICs rather than fitness
#'
#' @author Wei-Hsiang Sun
#'
#' @param datax a n by p matrix, where n is the number of observations, and p is the total number of variables in the data
#'
#' @param datay a length n numeric vector where n is the number of observations in the data
#'
#' @param P numeric, the size of population
#'
#' @param population a P by p matrix storing the population information, where P is the size of the population, and p is the gene length
#'
#' @param type character, the type of model to use, 'lm' for linear regression, 'glm' for logistic regression
#'
#' @return Returns a numeric vector of individual fitnesses of the population
#'
#' @examples # create dummy data
#' x <- matrix(rnorm(100),10,10)
#' y <- rnorm(10)
#' P = 5
#' population = matrix(rep(c(1,0),25),5,10)
#' type = 'lm'
#'
#' # run the fitness evaluation
#' fitness_aic(x,y,population,P,type)

fitness_aic <- function(datax,datay,population,P,type){

  assertthat::assert_that(is.matrix(datax))
  assertthat::assert_that(is.numeric(datay)|is.factor(datay))
  assertthat::assert_that(is.matrix(population))
  assertthat::assert_that(assertthat::is.number(P))
  assertthat::assert_that(is.character(type))

  # vector to store fitness for each individual
  AICs <- rep(0,P)

  # for logistic regression
  if(type == 'glm'){
    for(indiv in 1:P){
      glm <- glm(datay ~ datax[,(population[indiv,] > 0)],
                 family = "binomial")
      AICs[indiv] <- AIC(glm)
    }
    fitnesses <- (max(AICs)+1 - AICs)/sum(max(AICs) - AICs + .Machine$double.eps)
    return(fitnesses)
  }
  # for linear regression
  if(type == 'lm'){
    for(indiv in 1:P){
      lm <- lm(datay ~ datax[,(population[indiv,] > 0)])
      AICs[indiv] <- AIC(lm)
    }
    fitnesses <- (max(AICs)+1 - AICs)/sum(max(AICs) - AICs+ .Machine$double.eps)
    return(fitnesses)
  }
}

# AIC
AICs <- function(datax,datay,population,P,type){

  # vector to store fitness for each individual
  AICs <- rep(0,P)

  # for logistic regression
  if(type == 'glm'){
    for(indiv in 1:P){
      glm <- glm(datay ~ datax[,(population[indiv,] > 0)],
                 family = "binomial")
      AICs[indiv] <- AIC(glm)
    }
    return(AICs)
  }
  # for linear regression
  if(type == 'lm'){
    for(indiv in 1:P){
      lm <- lm(datay ~ datax[,(population[indiv,] > 0)])
      AICs[indiv] <- AIC(lm)
    }
    return(AICs)
  }
}


