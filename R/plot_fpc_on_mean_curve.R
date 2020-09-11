
#' A function to plot fpc on mean curves
#'
#' @param output The model output list from output_results() function
#' @param pc_idx The pc index to plot with
#' @param original The option to plot with original or transformed
#' time and response value
#' @export
plot_fpc_on_mean_curve <- function(output, pc_idx, original = FALSE){
  time_cont <- output$basis$time_cont
  Mu_functions <- output$Mu_functions
  FPC_mean <- output$FPC_mean
  data <- output$df

  if (original == TRUE){
    if ('time_ori' %in% colnames(data) & 'response_ori' %in% colnames(data)){
      response <- data$response_ori
      time <- data$time_ori
    } else {
      response <- data$response
      time <- data$time
    }
  } else {
    if ('time_ori' %in% colnames(data) & 'response_ori' %in% colnames(data)){
      response <- data$response
      time <- data$time
    } else {
      print('Time and response did not transformed. Print results with original values')
    }
  }

  sigma_y <- sd(response)
  mu_y <- mean(response)
  K <- output$rotation$npcs
  prop_var_avg <- output$rotation$prop_var_avg
  k <- pc_idx

  if (is.null(ymin) & is.null(ymax)) {
    ymin <- floor(min((Mu_functions + FPC_mean[, k]) * sigma_y + mu_y,
                    (Mu_functions - FPC_mean[, k]) * sigma_y + mu_y)) - 0.5
    ymax <- ceiling(max((Mu_functions + FPC_mean[, k]) * sigma_y + mu_y,
                      (Mu_functions - FPC_mean[, k]) * sigma_y + mu_y)) + 0.5
  }
  plot(time_cont, Mu_functions * sigma_y + mu_y, type="l", ylim=c(ymin, ymax),
       lwd=2,col=1, xlab='time', ylab='FPC Scores', font.lab=2, cex.lab=1.2)
  lines(time_cont, (Mu_functions + FPC_mean[, k]) * sigma_y + mu_y,
        type="l",lwd = 3,lty = 2,col = 2) # red
  lines(time_cont, (Mu_functions - FPC_mean[, k]) * sigma_y + mu_y,
        type="l",lwd = 3,lty = 2,col = 3) # green
  title(main=paste(paste('PC', k, sep=' '), ' (', prop_var_avg[k], ' )', sep=''))
  #axis(1, font=2) # make x-axis ticks label bold
  legend('topright', c('+ pc', '- pc'), lty=c(2,2), lwd=c(3,3), col=c(2, 3), bty='n', cex=0.5)
}
