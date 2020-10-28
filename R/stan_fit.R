#' S3 class sfpca model with number of pc, knot, sampling data from rstan,
#' log_liklihood, and leave-one-out cross-validation (LOO) information criterion
sfpcaClass <- function(Nsamples = NULL, Nchains = NULL, pc=NULL, knot=NULL,
                       sa=NULL, log_lik=NULL, looic=NULL, basis=NULL){
  sfpca_model <- list(
    Nsamples = Nsamples,
    Nchains = Nchains,
    pc = pc,
    knot = knot,
    sa = sa,
    log_lik = log_lik,
    looic = looic,
    basis = basis
  )

  ## Set the name for the class
  class(sfpca_model) <- append(class(sfpca_model),"sfpcaClass")
  return(sfpca_model)
}


#' generate sfpca models with different parameters
#'
#' @param sfpca_data: The prepared data list from prepare_data() function
#' @param Nsamples: Number of objects sampling from rstan
#' @param Nchains: Number of Markov chain using in rstan model
#' @param Ncores: Number of cores using in rstan model
#' @param PC_range: A vector of pc number
#' @param nknot_range: A vector of knot number
#' @return A list of sfpca classes with difference pc and knot numbers
#' @import loo
#' @import Rcpp
#' @import methods
#' @importFrom rstan sampling
#' @useDynLib BayesTime, .registration = TRUE
#' @export

stan_fit <- function(sfpca_data, Nsamples, Nchains, Ncores=NULL,
                     PC_range, nknot_range, seed=NULL){
  stan_results <- list()
  i <- 0
  for (k in PC_range) {
    for (d in nknot_range) {
      i <- i + 1
      sfpca <- sfpcaClass()
      sfpca$pc <- k
      sfpca$knot <- d
      print(paste('index i is:', i, 'number of PC:', k, 'number of knots:', d))
      results_basis <- basis_setup_sparse(sfpca_data = sfpca_data,
                                          nknots = d, orth = TRUE)
      sfpca$basis <- results_basis
      pca_data <- list(N = sfpca_data$num_subjects,
                       K = k,
                       Q = d + 4,
                       Y = sfpca_data$response.list,
                       V = sfpca_data$visits.vector,
                       subject_starts = sfpca_data$visits.start,
                       subject_stops = sfpca_data$visits.stop,
                       cov_starts = sfpca_data$cov.start,
                       cov_stops = sfpca_data$cov.stop,
                       cov_size = sfpca_data$cov.size,
                       B = results_basis$orth_spline_basis_sparse_stacked)

      if (is.null(Ncores)) Ncores <- getOption("mc.cores", 1L)
      if (is.null(seed)) seed <- 31
      set.seed(seed)
      sa <- rstan::sampling(stanmodels$sfpca, data = pca_data, iter = Nsamples,
                            chains = Nchains, cores = Ncores, init = "random")
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
