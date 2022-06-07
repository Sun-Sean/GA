#' Mutation
#' Calculate the fitness of each individual in the population
#'
#' @author Wei-Hsiang Sun
#'
#' @param mat an n by p matrix of offsprings, where n is the number of individuals and p is the length of the gene
#'
#' @param mu numeric, the probability of mutation
#'
#' @return returns the mutated offspring population matrix
#'
#' @examples # create matrix, and perform mutation
#' x <- matrix(rep(c(1,0),5),5,2)
#' # perform mutation
#' mutation(mat=x,mu=0.5)

mutation <- function(mat, mu){
  # insure input
  assertthat::assert_that(is.matrix(mat))
  assertthat::assert_that(is.numeric(mu))

  # dimension of the dataframe
  row <- dim(mat)[1]
  col <- dim(mat)[2]

  # get the mutation points in matrix randomly
  # uniform 0 1 datapoints are compared to threshold mu
  MutationPoints <- runif(row*col) < mu

  # flip bits where mutation happens
  mat[MutationPoints] <- 1 - mat[MutationPoints]

  # return result
  return(mat)

}

