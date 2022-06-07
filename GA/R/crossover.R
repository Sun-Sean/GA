#' Crossover
#'
#' This function takes genes from two parents based on the selected pool and produces a new offspring
#' @author Lulude Sun
#' 
#' @param parents a n by 2 matrix consisting of the indices of the selected parents from the original dataframe
#' @param parents_info info matrix containing all the information of all parents
#' @examples # Generate a random population and crossover to create a new population
#' population <- sample(100:200, 1)
#' n <- sample(10:20, 1)
#' chromosomes <- matrix(rbinom(population*n,1, 0.5), population, n)
#' pairs <- matrix(sample(1:population, 2*population, replace = TRUE), population, 2)
#' offsprings <- crossover(pairs, chromosomes)

crossover <- function(parents, parents_info){

  # Check if the userinput is the correct format

  assertthat::assert_that(is.matrix(parents))
  assertthat::assert_that(is.numeric(parents_info))

  # Get the total number of pairs of parents
  n <- dim(parents)[1]
  # Get the length of the chromosomes
  m <- ncol(parents_info)
  offsprings <- matrix(NA,n,m)

  for(i in 1:n){

    # Obtain the genetic information of each pair of parents
    parent1 = parents_info[parents[i,1],]
    parent2 = parents_info[parents[i,2],]

    # Generate a random crossover point
    index = sample(1:(m-1),1)

    # Crossing over at a single point to get one offspring
    offspring = c(parent1[1:index], parent2[(index+1):m])

    # Update the genetic information of the newly produced offspring
    offsprings[i,] = offspring
  }

  return(offsprings)
}
