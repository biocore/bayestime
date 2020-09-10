#' generate sfpca models with different parameters
#'
#' @param da_list: The prepared data from prepare_data() function
#' @param model: The optimal model from optimal() function
#' @param variables: A set of variable names insterested in original data
#' @return A list of results from model
#' @importFrom Matrix bdiag
#' @export
output_results <- function(da_list, model){
  basis <- basis_setup_sparse(da_list, nknots = model$knot, orth=TRUE)
  rotation <- post_hoc_rotation(da_list, model = model)

  npcs <- rotation$npcs
  ALPHA_array <- rotation$alpha_new
  MU_array <- rotation$theta_mu_new
  THETA_array <- rotation$Theta_new
  phi_t_cont <- basis$orth_spline_basis_cont
  phi_t <- basis$orth_spline_basis_sparse
  time_cont <- basis$time_cont
  N <- da_list$num_subjects

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

  Mu_functions_temp <- Matrix::bdiag(cbind(phi_t_cont))
  Mu_functions <- t(as.matrix(Mu_functions_temp)) %*% MU_mean
  FPC_mean <- t(phi_t_cont) %*% THETA_mean

  if (('time_ori' %in% colnames(da_list$data)) & ('response_ori' %in% colnames(da_list$data))){
    var_require <- c('ID', 'time', 'time_ori', 'response', 'response_ori')
  } else {
    var_require <- c('ID', 'time', 'response')
  }
  vars_complete <- c(var_require, colnames(da_list$data)[!colnames(da_list$data) %in% var_require])
  df <- da_list$data[, vars_complete]
  # tryCatch({
  #   df <- da_list$data[, vars_complete]
  # }, error = function(e){
  #   cat("ERROR :", 'Input variables are not in data', "\n")
  # })

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
                  basis = basis,
                  rotation = rotation,
                  Mu_functions = Mu_functions,
                  time_sparse = time_sparse,
                  Y_sparse = Y_sparse,
                  FPC_mean = FPC_mean)
  return(results)
}
