#' plot group mean of fpc scores
#'
#' @param output: The model output list from output_results() function
#' @param group_name One column name of interested group variables in data
#' @export
plot_fpc_group_mean <- function(output, group_name){
  data <- output$df
  K <- output$rotation$npcs

  if ('time_ori' %in% colnames(data) & 'response_ori' %in% colnames(data)){
    response <- data$response_ori
    time <- data$time_ori
  } else {
    response <- data$response
    time <- data$time
  }

  sigma_y <- sd(log(response))
  mu_y <- mean(log(response))
  time_cont <- output$basis$time_cont
  Y_sparse <- output$Y_sparse
  Mu_functions <- output$Mu_functions
  time_sparse <- output$time_sparse
  FPC_mean <- output$FPC_mean
  prop_var_avg <- output$rotation$prop_var_avg

  ymin <- floor(min(unlist(Y_sparse) * sigma_y + mu_y,
                    Mu_functions * sigma_y + mu_y)) - 0.1
  ymax <- ceiling(max(unlist(Y_sparse) * sigma_y + mu_y,
                      Mu_functions * sigma_y + mu_y)) + 0.1

  for (k in 1:K){
    fpcs <- paste('fpc', k, sep = '')
    data_temp <- data[, c('ID', group_name, fpcs)]
    classes <- levels(data_temp[, group_name])
    groups <- length(classes)
    scores_mu_g <- numeric(groups)
    for (j in 1:groups){

    }

    plot(time_cont * max(time), Mu_functions * sigma_y + mu_y, type="n",
         lwd = 2, xlab='Time', ylab='Response', font.lab = 2, cex.lab = 1.2,
         ylim = c(ymin, ymax))

    for (j in 1:groups){
      scores_mu_g_temp = mean(data_temp[data_temp[, group_name] == classes[j], fpcs])
      lines(time_cont * max(time),
            (Mu_functions + FPC_mean[, k] * scores_mu_g_temp) * sigma_y + mu_y,
            type = "l", lwd = 3, lty = 2, col = j+1)
    }

    title(main=paste(paste('PC', k, sep=' '), ' (', prop_var_avg[k], ' )', sep=''))
    legend('bottomright', classes, lty = rep(2, groups), lwd = rep(3, groups),
           col = seq(2, groups + 1), bty='n', cex=0.5)
  }
}
