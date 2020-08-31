#' A function to plot fpc curves
#'
#' @param data A data frame for longitudinal analysis
#' @param response_name The name of response variable
#' @param basis The model basis results list from basis_setup_sparse() function
#' @param output The model output list from output_results() function
#' @export
plot_fpc_curve <- function(data, response_name, basis, output, rotation){
  time_cont <- basis$time_cont
  Mu_functions <- output$Mu_functions
  FPC_mean <- output$FPC_mean
  response <- data[, response_name]
  sigma_y <- sd(log(response))
  mu_y <- mean(log(response))
  K <- rotation$npcs
  prop_var_avg <- rotation$prop_var_avg
  for (k in 1:K) {
    mi <- floor(min((Mu_functions + FPC_mean[, k]) * sigma_y + mu_y,
                    (Mu_functions - FPC_mean[, k]) * sigma_y + mu_y)) - 0.5
    ma <- ceiling(max((Mu_functions + FPC_mean[, k]) * sigma_y + mu_y,
                      (Mu_functions - FPC_mean[, k]) * sigma_y + mu_y)) + 0.5
    plot(time_cont, Mu_functions * sigma_y + mu_y, type="l", ylim=c(mi, ma),
         lwd=2,col=1, xlab='Month of life', ylab='Shannon diversity', font.lab=2, cex.lab=1.2)
    lines(time_cont, (Mu_functions + FPC_mean[, k]) * sigma_y + mu_y,
          type="l",lwd = 3,lty = 2,col = 2) # red
    lines(time_cont, (Mu_functions - FPC_mean[, k]) * sigma_y + mu_y,
          type="l",lwd = 3,lty = 2,col = 3) # green
    title(main=paste(paste('PC', k, sep=' '), ' (', prop_var_avg[k], ' )', sep=''))
    #axis(1, font=2) # make x-axis ticks label bold
    legend('topright', c('+ pc', '- pc'), lty=c(2,2), lwd=c(3,3), col=c(2, 3), bty='n', cex=0.5)
  }
}
