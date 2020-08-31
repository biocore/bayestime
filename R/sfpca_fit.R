#' S3 class sfpca model with number of pc, knot, sampling data from rstan,
#' log_liklihood, and leave-one-out cross-validation (LOO) information criterion
sfpcaClass <- function(Nsamples = NULL, Nchains = NULL, pc=NULL, knot=NULL,
                       sa=NULL, log_lik=NULL, looic=NULL){
  sfpca_model <- list(
    Nsamples = Nsamples,
    Nchains = Nchains,
    pc = pc,
    knot = knot,
    sa = sa,
    log_lik = log_lik,
    looic = looic
  )

  ## Set the name for the class
  class(sfpca_model) <- append(class(sfpca_model),"sfpcaClass")
  return(sfpca_model)
}


#' generate sfpca models with different parameters
#'
#' @param PC_max: Number of the maximum principal component
#' @param D_max: Number of the maximum knot
#' @param Nsamples: Number of objects sampling from rstan
#' @param Nchains: Number of Markov chain using in rstan model
#' @param smod: Stan model constructed from source file (rstan)
#' @param sfpca_data: The prepared data list from prepare_data() function
#' @return A list of sfpca classes with difference pc and knot numbers
#' @import loo
#' @import Rcpp
#' @import methods
#' @importFrom rstan sampling
#' @useDynLib BayesTime, .registration = TRUE
#' @export

stan_fit <- function(data, Nsamples, Nchains, PC_max, D_max){
  sfpca_results <- list()
  i <- 0
  for (k in 1:PC_max) {
    for (d in 1:D_max) {
      i <- i + 1
      sfpca <- sfpcaClass()
      sfpca$pc <- k
      sfpca$knot <- d
      print(paste('index i is:', i, 'number of PC:', k, 'number of knots:', d))
      results_basis <- basis_setup_sparse(sfpca_data = data,
                                          nknots = d, orth = TRUE)
      pca_data <- list(N = data$num_subjects,
                       K = k,
                       Q = d + 4,
                       Y = data$response.list,
                       V = data$visits.vector,
                       subject_starts = data$visits.start,
                       subject_stops = data$visits.stop,
                       cov_starts = data$cov.start,
                       cov_stops = data$cov.stop,
                       cov_size = data$cov.size,
                       B = results_basis$orth_spline_basis_sparse_stacked)

      set.seed(31)
      sa <- rstan::sampling(stanmodels$sfpca, data = pca_data, iter = Nsamples,
                            chains = Nchains, init = "random")
      sfpca$sa <- sa
      sfpca$log_lik <- rstan::extract(sa,"log_lik_marg")[[1]]
      sfpca$looic <- loo::loo(sfpca$log_lik)
      sfpca$Nsamples <- Nsamples
      sfpca$Nchains <- Nchains
      sfpca_results[[i]] <- sfpca
      print("######### SFPCA ###############")
    }
  }
  return(sfpca_results)
}
