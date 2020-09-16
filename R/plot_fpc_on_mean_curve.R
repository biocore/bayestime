#' A function to plot fpc on mean curves
#'
#' @param output The model output list from output_results() function
#' @param pc_idx The pc index to plot with
#' @param original The option to plot with original or transformed
#' time and response value
#' @param sd: The option to include one standard deviation of fpc scores
#' @param x_lab: Manually set x axis title
#' @param y_lab: Manually set y axis title
#' @param ymin The minimum of y lab
#' @param ymax The maximum of y lab
#' @return A list with the plot and the data used for plot
#' @import reshape
#' @export
plot_fpc_on_mean_curve <- function(output, pc_idx,
                                   original = FALSE, sd = FALSE,
                                   x_lab=NULL, y_lab=NULL,
                                   ymin=NULL, ymax=NULL){
  time_cont <- output$basis$time_cont
  Mu_functions <- output$Mu_functions
  FPC_mean <- output$FPC_mean
  data <- output$df

  if (original == TRUE){
    response <- data$response_ori
    time <- data$time_ori
  } else {
    response <- data$response
    time <- data$time
    if (all(data$response == data$response_ori) &
        all(data$time == data$time_ori)){
      print('Warning: Response and time are not transformed.
            Plot with original values')
    }
  }

  sigma_y <- sd(response)
  mu_y <- mean(response)
  K <- output$rotation$npcs
  prop_var_avg <- output$rotation$prop_var_avg
  k <- pc_idx

  if (is.null(ymin)) {
    ymin <- floor(min((Mu_functions + FPC_mean[, k]) * sigma_y + mu_y,
                    (Mu_functions - FPC_mean[, k]) * sigma_y + mu_y)) - 0.5 }
  if (is.null(ymax)) {
    ymax <- ceiling(max((Mu_functions + FPC_mean[, k]) * sigma_y + mu_y,
                      (Mu_functions - FPC_mean[, k]) * sigma_y + mu_y)) + 0.5
  }
  if (is.null(x_lab)) x_lab = 'time'
  if (is.null(y_lab)) y_lab = 'response'

  plot_data <- data.frame(time_cont * (max(time) - min(time)) + min(time))
  colnames(plot_data) <- 'time'
  if (sd == TRUE) {
    plot_data$response <- Mu_functions * sd(FPC_mean) * sigma_y + mu_y
    plot_data$pc_plus <- (Mu_functions + FPC_mean[, k]) *
      sd(FPC_mean) * sigma_y + mu_y
    plot_data$pc_minus <-  (Mu_functions - FPC_mean[, k]) *
      sd(FPC_mean) * sigma_y + mu_y

  } else {
    plot_data$response <- Mu_functions * sigma_y + mu_y
    plot_data$pc_plus <- (Mu_functions + FPC_mean[, k]) * sigma_y + mu_y
    plot_data$pc_minus <- (Mu_functions - FPC_mean[, k]) * sigma_y + mu_y
  }
  plot_melt <- reshape::melt(data = plot_data, id.vars = c("time"),
                    measure.vars = colnames(plot_data)[-1])
  plot_melt$type <- factor(ifelse(plot_melt$variable != y_lab, 'pcs', y_lab),
                           levels = c(y_lab, 'pcs'))
  p <- ggplot() +
    geom_line(data=plot_melt, aes(x = time, y = value,
                                  color = variable,
                                  linetype = type), lwd = 1) +
    guides(linetype = F) +
    labs(title= paste(paste('PC', k, sep=' '), ' (',
                      prop_var_avg[k], ' )', sep=''),
         colour = 'curves', x = x_lab, y = y_lab) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.text.x = element_text(size = 10, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold")) +
    scale_color_manual(values = c(response = "black",
                                  pc_plus = "orange",
                                  pc_minus = "blue" ))
  print(p)
  return(results <- list('data' = plot_data,
                         'plot' = p))
  # plot(time_cont * (max(time) - min(time)) + min(time),
  #       Mu_functions * sigma_y + mu_y,
  #      type = "l", ylim = c(ymin, ymax), lwd = 2,col = 1,
  #      xlab = xlab, ylab = ylab, font.lab = 2, cex.lab = 1.2)
  #
  # lines(time_cont * (max(time) - min(time)) + min(time),
  #       (Mu_functions + FPC_mean[, k]) * sigma_y + mu_y,
  #       type = "l",lwd = 3,lty = 2,col = 2) # red
  #
  # lines(time_cont * (max(time) - min(time)) + min(time),
  #       (Mu_functions - FPC_mean[, k]) * sigma_y + mu_y,
  #       type = "l",lwd = 3,lty = 2,col = 3) # green
  # title(main  = paste(paste('PC', k, sep=' '), ' (',
  #                     prop_var_avg[k], ' )', sep=''))
  # #axis(1, font=2) # make x-axis ticks label bold
  # legend('topright', c('+ pc', '- pc'),
  #        lty = c(2, 2), lwd = c(3, 3), col = c(2, 3), bty = 'n', cex = 0.5)
}
