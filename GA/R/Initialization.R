#' Initialization
#'
#' Generate a random population as the first chromosome generation
#'
#' @return a population matrix
#'
#' @author Wei-Hsiang Sun
#'
#' @param p numeric, the population size
#'
#' @param datax a n by p matrix or dataframe, n is the number of observations and p is the number of variables in the original data
#'
#' @examples # create sample data
#' x <- matrix(100,10,10)
#' p <- 10
#'
#' # initialize
#' init(p,x)

# assume data is n (no. of obs.) by p (no. of variables)
init <- function(p,datax){

  assertthat::assert_that(assertthat::is.number(p))
  assertthat::assert_that(is.matrix(datax)|is.data.frame(datax))

  # get the number of variables
  gene_length <- dim(datax)[2]

  # create matrix
  mat <- sample(c(0,1),size = gene_length*p, replace = TRUE)
  dim(mat) <- c(p,gene_length)

  # make sure no row is all zero
  while(any(rowSums(mat) < 1)){
    index_zeros <- which(rowSums(mat) < 1)[1]
    mat[index_zeros,] <- sample(c(0,1),size=gene_length,replace = TRUE)
  }

  # return matrix
  mat
}

