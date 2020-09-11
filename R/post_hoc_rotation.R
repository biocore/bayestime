#' perform post hoc rotation
#'
#' @param da_list: The prepared data from prepare_data() function
#' @param model The optimal sfpca model
#' @return A list after post hoc rotation (***)
#' @import rstan
#' @export
post_hoc_rotation <- function(da_list, model){
  sa <- model$sa
  Sigma <- rstan::extract(sa, "Sigma", permuted=FALSE)
  W <- rstan::extract(sa, "W", permuted=FALSE)
  sigma_eps <- rstan::extract(sa, "sigma_eps", permuted=FALSE)
  theta_mu <- rstan::extract(sa, "theta_mu", permuted=FALSE)
  alpha <- rstan::extract(sa, "alpha", permuted=FALSE)
  Theta <- rstan::extract(sa, "Theta", permuted=FALSE)

  ## Reshape parameters and reorient loadings with PCA rotation
  N <- da_list$num_subjects
  K <- model$pc
  Q <- model$knot + 4
  Nchains <- model$Nchains
  Nsamples <- model$Nsamples

  theta_mu_new <- array(0, dim = c(Q, Nchains * Nsamples / 2))
  alpha_old <- alpha_new <- array(0, dim = c(K, N, Nchains * Nsamples / 2))
  Theta_old <- Theta_new <- array(0, dim = c(Q, K, Nchains * Nsamples / 2))
  W_old <- array(0, dim = c(Q, Q, Nchains * Nsamples / 2))

  ind <- 0
  prop_var <- NULL
  for (i in 1:dim(W)[1]) {
    for (j in 1:dim(W)[2]) {
      #print(ind)
      ind <- ind + 1
      theta_mu_new[, ind] <- array(theta_mu[i, j, ])
      alpha_old[, , ind] <- t(array(alpha[i,j,],dim = c(N, K)))
      Theta_old[, , ind] <- array(Theta[i,j,],dim = c(Q, K))
      W_old[,,ind] <- array(W[i, j, ], dim = c(Q, Q))

      eigen_temp_sigma <- eigen(W_old[, , ind])
      v_temp <- eigen_temp_sigma$vectors
      d_temp <- eigen_temp_sigma$values
      prop_var <- rbind(prop_var, d_temp / sum(d_temp)) # proportion of variance explained by each PC

      for (com in 1:length(d_temp)) {
        if (!(d_temp[com] - Re(d_temp[com]) == 0)) {
          d_temp[com] <- -1 * 10 ^ 5
        }
      }
      pos_temp <- array(0, dim = c(K, 1))
      for (pos in 1:K) {
        pos_temp[pos] <- (1:length(d_temp))[max(d_temp) == d_temp]
        d_temp[pos_temp[pos]] <- -1e+5
      }

      Theta_new[, , ind] <- v_temp[, pos_temp]
      for (k in 1:K) {
        Theta_new[, k, ind] <- sign(Theta_new[1, k, ind]) * Theta_new[, k, ind]
      }

      alpha_new[, , ind] <- t(Theta_new[, , ind]) %*% Theta_old[, , ind] %*% alpha_old[, , ind]
    }
  }
  prop_var_avg_origin <- colMeans(prop_var)
  (prop_var_avg <- paste(round(colMeans(prop_var) * 100, 2), '%', sep=''))
  #rename Q
  rotation <- list(num_subjects = N,
                   npcs = K,
                   nknots = model$knot,
                   Q = Q,
                   alpha_new = alpha_new,
                   theta_mu_new = theta_mu_new,
                   Theta_new = Theta_new,
                   prop_var_avg_origin = prop_var_avg_origin,
                   prop_var_avg = prop_var_avg)
  return(rotation)
}
