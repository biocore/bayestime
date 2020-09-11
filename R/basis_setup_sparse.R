#' set up spline basis for sparse data
#'
#' @param da_list longitudinal data generated from prepare_data() function (list)
#' @param nknots: user-defined number of knots
#' @param orth: default setting for orth should be TRUE (after discussed with Wes on 02/13/2019)
#' @param delta: set to be 1/10000 to avoid rounding error(need eddit)
#' @return A list containing the spline basis of sparse data
#' @import splines
#' @export

basis_setup_sparse = function(da_list, nknots, orth=TRUE, delta = 1/10000){
  #set up variables
  time_var <- da_list$data$time
  num_subjects <-da_list$num_subjects
  num_times <- da_list$num_times
  S <- da_list$time.matrix
  V <- da_list$visits.vector
  # continuous time interval
  time_unique <- sort(unique(time_var))
  time_min <- min(time_unique)
  time_max <- max(time_unique)
  time_cont <- seq(time_min, time_max / delta) * delta # chop the entire time interval into many small subintervals
  time_cont <- round(time_cont / delta) * delta # to avoid rounding error?

  # specify placement of knots
  qs <- 1 / (nknots + 1)
  knots <- quantile(time_unique, qs)
  if(nknots > 1){
    for(q in 2:nknots){
      knots <- c(knots, q * quantile(time_unique, qs))
    }
  }

  knots <- as.vector(knots)


  # obtain cubic spline basis
  phi_t_cont <- list()
  phi_t_cont <- splines::bs(time_cont, knots = knots, degree = 3,
                            intercept = TRUE) # cubic spline, degree=spline_degree

  ### the same as in setup_basis_sparse so far

  # ## 1. for densely sampled time points
  # Gram-Schmidt Orthonormalization
  temp <- phi_t_cont
  K <- nknots + 4 # num of spline basis

  for (k in 1:K) {
    if (orth==TRUE) {
      if (k > 1) {
        for (q in 1:(k - 1)) {
          temp[, k] <- temp[, k] - (sum(temp[, k] * temp[, k - q]) /
                               sum(temp[, k - q] ^ 2)) * temp[, k - q];
        }
      }
    }
    temp[, k] <- temp[, k] / sqrt(sum(temp[, k] * temp[, k]))
  }

  phi_t_cont <- t(sqrt(1 / delta) * temp)

  ## 2. for sparsely sampled time points
  phi_t_stacked <- NULL
  phi_t <- list()
  for (i in 1:num_subjects) {
    phi_t[[i]] <- array(0, dim = c(K, V[i])) # phi_t: K (number of basis function) * number of total visit for each subject

    for (k in 1:K) {
      for (t in 1:V[i]) {
        phi_t[[i]][k, t] <- phi_t_cont[k, abs(time_cont - S[i, t]) == min(abs(time_cont - S[i, t]))]
      }
    }

    # stack subjects and visits: number of visits as rows, and number of basis as columns
    phi_t_stacked <- rbind(phi_t_stacked, t(phi_t[[i]]))
  }

  results_basis <- list(knot_place = knots, time_cont = time_cont,
                        orth_spline_basis_sparse = phi_t,
                        orth_spline_basis_sparse_stacked = phi_t_stacked,
                        orth_spline_basis_cont = phi_t_cont)
  return(results_basis)
}
