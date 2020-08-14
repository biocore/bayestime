#' generate sfpca models with different parameters
#'
#' @param data: The prepared data from prepare_data() function
#' @param variables: A set of variable names insterested in original data
#' @param basis: The list of basis results from basis_setup_sparse()
#' @param rotation: The list of rotation results from post_hoc_rotation()
#' @return A list of results from sfpca model and function
output_results <- function(sfpca_data, variables, basis, rotation){
  npcs <- rotation$npcs
  ALPHA_array <- rotation$alpha_new
  MU_array <- rotation$theta_mu_new
  THETA_array <- rotation$Theta_new
  phi_t_cont <- basis$orth_spline_basis_cont
  phi_t <- basis$orth_spline_basis_sparse
  time_cont <- basis$time_cont
  N <- sfpca_data$num_subjects

  nloop <- dim(ALPHA_array)[3]
  first <- 1
  last <- nloop

  MU_mean <- MU_array[, first] #mean function across sampling sessions
  ALPHA_mean <- ALPHA_array[, , first] # mean factor scores
  THETA_mean <- THETA_array[, , first] # mean factor loading

  for (iter in 2:nloop) {
    MU_mean <- MU_mean + MU_array[, iter]
    ALPHA_mean <- ALPHA_mean + ALPHA_array[, , iter]
    THETA_mean <- THETA_mean + THETA_array[, , iter]
  }

  MU_mean <- cbind(MU_mean / (last - first + 1))
  ALPHA_mean <- cbind(ALPHA_mean / (last - first + 1))
  THETA_mean <- cbind(THETA_mean / (last - first + 1))

  Mu_functions <- t(bdiag(cbind(phi_t_cont))) %*% MU_mean
  FPC_mean <- t(phi_t_cont) %*% THETA_mean

  vars_complete <- c('ID', 'time', 'response', variables)
  tryCatch({
    df <- sfpca_data$data[, vars_complete]
  }, error = function(e){
    cat("ERROR :", 'Selected variables not in data', "\n")
  })
  Y_sparse <- list()
  time_sparse <- list()
  scores <- data.frame(t(ALPHA_mean))
  if (npcs == 1) {
    ### create data frame containing needed information ####
    df$fpc1 <- 0 # principle component scores

    i <- 0
    for (pid in unique(df$ID)) {
      i <- i + 1
      Y_sparse[[i]] <- df$response[df$ID == pid]
      time_sparse[[i]] <- df$time[df$ID == pid]
      df$fpc1[df$ID == pid] <- scores[i]
    }
    df$fpc1 <- as.numeric(df$fpc1) # data type issue

    Fits_sparse <- list()
    for (i in 1:N) {
      Fits_sparse[[i]] <- t(phi_t[[i]]) %*% MU_mean + t(phi_t[[i]]) %*% THETA_mean %*% ALPHA_mean[i]
    }

    df$Y_sparse <- unlist(Y_sparse)
    df$Fits_sparse <- unlist(Fits_sparse)
    df$residuals <- df$Y_sparse - df$Fits_sparse
  } else {
    ### create data frame containing needed information ####
    for (k in 1:npcs) {
      names(scores)[k] <- paste('fpc', k, sep = '')
      df[names(scores)[k]] <- 0 # principle component scores  # it depends of PCs (better to choose number of PCs as input)
    }

    i <- 0
    for (pid in unique(df$ID)) {
      i <- i + 1
      Y_sparse[[i]] <- df$response[df$ID == pid]
      time_sparse[[i]] <- df$time[df$ID == pid]
      for (k in 1:npcs) {
        df[, names(scores)[k]][df$ID == pid] <- scores[i, k]
      }
    }

    Fits_sparse <- list()
    for (i in 1:N) {
      Fits_sparse[[i]] <- t(phi_t[[i]]) %*% MU_mean + t(phi_t[[i]]) %*%
        THETA_mean %*% ALPHA_mean[, i]
    }

    df$Y_sparse <- unlist(Y_sparse) # check: sum(df$Y_sparse != df$response) == 0
    df$Fits_sparse <- unlist(Fits_sparse)
    df$residuals <- df$Y_sparse - df$Fits_sparse
  }

  results <- list(df = df,
                  Mu_functions = Mu_functions,
                  time_sparse = time_sparse,
                  Y_sparse = Y_sparse,
                  FPC_mean = FPC_mean)
  return(results)
}
