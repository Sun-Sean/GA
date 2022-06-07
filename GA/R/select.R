#' Select
#' A function that outputs the best subset of variables according to the aic criteria. User can select different fitness functions as well as different types of selection techniques.
#'
#' @return A list of two elements, the best aic value, and the best subset of variables. Variables selected corresponds to 1 in a numeric vector, while unselected variables are indexed 0.
#'
#' @author Wei-Hsiang Sun
#'
#' @param datax a n by m matrix, where n is the number of observations, and m is the total number of variables in the data
#'
#' @param datay a length n numeric vector where n is the number of observations in the data
#'
#' @param p numeric, the size of population
#'
#' @param mu numeric, has value between 0 and 1, mutation probability, is set to 0.01 by default
#'
#' @param selection_type character, the type of selection to use, 'twoprop' for proportional selection for both parents, 'oneprop' for proportional selection for one parent, 'tournament' for tournament selection, 'rank' for rank based selection, 'stochastic' for random selection. by default, the selection_type is set to 'twoprop'
#'
#' @param type character, the type of model to use, 'lm' for linear regression, 'glm' for logistic regression
#'
#' @param max_iter integer, maximum iteration, is set to 100 by default
#'
#' @param k integer, groups for tournament selection
#'
#' @param threshold numeric, the converging threshold, the criteria for convergence between and within population, threshold is set to 10^-5 by default
#'
#' @examples # import library to create toy data for glm example
#' library("VSURF")
#' data("toys")
#' set.seed(1)
#' x <- toys[['x']]
#' y <- toys[['y']]
#'
#' # selection results
#' select(x,y,p=10,selection_type = 'tournament',max_iter = 100,model_type = 'glm',k=5,threshold = 10^-3 )
#'
#' # create data for lm example
#' set.seed(5)
#' x <- rnorm(1000,3,2)
#' dim(x) <- c(100,10)
#' y <- 3*x[,1] + 4*x[,7]- 5*x[,9] + rnorm(100)
#'
#' # selection results
#' select(x,y,p=10,selection_type = 'tournament',max_iter = 100,k=5,threshold = 10^-4 )
#'
#' @export
#'
#' @source(".R/Mutation.R")
#' @source(".R/Fitness.R")
#' @source(".R/selection.R")
#' @source(".R/crossover.R")
#' @source(".R/Initialization.R")

select <- function(datax,datay,p,mu=0.01,
                   selection_type='twoprop',
                   model_type='lm',
                   max_iter = 100,
                   fitness_func = fitness_aic,
                   k=NULL,
                   threshold = 10^-5){

  # generate initial population
  population <- init(p,datax)

  # convert datatype
  datax <- as.matrix(datax)

  if(is.null(fitness_func)){
    fitness_func <- fitness_aic
  }

  # get number of features
  gene_length <- dim(datax)[2]

  # convergence criteria
  converge <- FALSE

  # aic of previous population
  aic_prev <- AICs(datax,datay,population,p,model_type)

  # aic of current population
  aic_curr <- aic_prev

  # begin iteration
  for(iter in 1:max_iter){

    # get fitness
    fitnesses <- fitness_func(datax,datay,population,p,model_type)

    # begin selection
    if(selection_type == 'twoprop'){
      parents <- twopropselection(fitnesses)
    }else if(selection_type == 'oneprop'){
      parents <- onepropselection(fitnesses)
    }else if(selection_type == 'tournament'){
      parents <- tournament(fitnesses,k)
    }else if(selection_type == 'rank'){
      parents <- rankselection(rank(-fitnesses))
    }else if(selection_type == 'stochastic'){
      parents <- sus(fitnesses)
    }

    # crossover
    offsprings <- crossover(parents,population)

    # mutation
    population <- mutation(offsprings,mu)

    # prevent all zeros
    while(any(rowSums(population) < 1)){
      index_zeros <- which(rowSums(population) < 1)[1]
      population[index_zeros,] <- population[sample(1:gene_length,1),]
    }
    AICs <- AICs(datax,datay,population,p,model_type)
    cat('iteration',iter,'\n')
    cat('min AIC',min(AICs),'\n')
    cat('max AIC',max(AICs),'\n')
    cat('quantiles',quantile(AICs),'\n')

    # update aic population
    aic_prev <- aic_curr
    aic_curr <- AICs

    # see whether converge
    cat('between population convergence',abs(min(aic_prev) - min(aic_curr)) < min(aic_curr)*threshold,'\n')
    cat('within population convergence',abs(mean(aic_curr) - min(aic_curr)) < median(aic_curr)*threshold,'\n\n')
    converge <- (abs(min(aic_prev) - min(aic_curr)) < min(aic_curr)*threshold) &
      (abs(mean(aic_curr) - min(aic_curr)) < median(aic_curr)*threshold)

    # Check convergence criteria
    if(converge){
      fitnesses <- fitness_func(datax,datay,population,p,model_type)
      fittest_individual <- which.max(fitnesses)
      return(list('aic' = min(AICs),
                  'individual' = population[fittest_individual,]))
    }
  }
  fitnesses <- fitness_func(datax,datay,population,p,model_type)
  fittest_individual <- which.max(fitnesses)
  return(list('aic' = min(AICs),
              'individual' = population[fittest_individual,]))
}


