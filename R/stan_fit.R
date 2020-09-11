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
#' @param da_list: The prepared data list from prepare_data() function
#' @param Nsamples: Number of objects sampling from rstan
#' @param Nchains: Number of Markov chain using in rstan model
#' @param PC_range: A vector of pc number
#' @param nknot_range: A vector of knot number
#' @return A list of sfpca classes with difference pc and knot numbers
#' @import loo
#' @import Rcpp
#' @import methods
#' @importFrom rstan sampling
#' @useDynLib BayesTime, .registration = TRUE
#' @export

stan_fit <- function(da_list, Nsamples, Nchains, PC_range, nknot_range){
  stan_results <- list()
  i <- 0
  for (k in PC_range) {
    for (d in nknot_range) {
      i <- i + 1
      sfpca <- sfpcaClass()
      sfpca$pc <- k
      sfpca$knot <- d
      print(paste('index i is:', i, 'number of PC:', k, 'number of knots:', d))
      results_basis <- basis_setup_sparse(da_list = da_list,
                                          nknots = d, orth = TRUE)
      pca_data <- list(N = da_list$num_subjects,
                       K = k,
                       Q = d + 4,
                       Y = da_list$response.list,
                       V = da_list$visits.vector,
                       subject_starts = da_list$visits.start,
                       subject_stops = da_list$visits.stop,
                       cov_starts = da_list$cov.start,
                       cov_stops = da_list$cov.stop,
                       cov_size = da_list$cov.size,
                       B = results_basis$orth_spline_basis_sparse_stacked)

      set.seed(31)
      sa <- rstan::sampling(stanmodels$sfpca, data = pca_data, iter = Nsamples,
                            chains = Nchains, init = "random")
      sfpca$sa <- sa
      sfpca$log_lik <- rstan::extract(sa,"log_lik_marg")[[1]]
      sfpca$looic <- loo::loo(sfpca$log_lik)
      sfpca$Nsamples <- Nsamples
      sfpca$Nchains <- Nchains
      stan_results[[i]] <- sfpca
      print("######### SFPCA ###############")
    }
  }
  return(stan_results)
}
