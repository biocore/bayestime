#' plot group mean of fpc scores
#'
#' @param output: The model output list from output_results() function
#' @param pc_idx The pc index to plot with
#' @param group_name One column name of interested group variables in data
#' @param original The option to plot with original or transformed
#' @param x_lab: Manually set x axis title
#' @param y_lab: Manually set y axis title
#' @param ymin The minimum of y lab
#' @param ymax The maximum of y lab
#' @return A list with the plot and the data used for plot
#' @import reshape
#' @export
plot_fpc_group_mean <- function(output, pc_idx, original = FALSE, group_name,
                                x_lab = NULL, y_lab = NULL,
                                ymin = NULL, ymax = NULL){
  data <- output$df
  data[, group_name] <- as.factor(data[, group_name])
  K <- output$rotation$npcs

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
  time_cont <- output$basis$time_cont
  Y_sparse <- output$Y_sparse
  Mu_functions <- output$Mu_functions
  time_sparse <- output$time_sparse
  FPC_mean <- output$FPC_mean
  prop_var_avg <- output$rotation$prop_var_avg

  if (is.null(ymin)) {
    ymin <- floor(min(unlist(Y_sparse) * sigma_y + mu_y,
                      Mu_functions * sigma_y + mu_y)) - 0.1 }
  if (is.null(ymax)) {
    ymax <- ceiling(max(unlist(Y_sparse) * sigma_y + mu_y,
                        Mu_functions * sigma_y + mu_y)) + 0.1 }
  if (is.null(x_lab)) x_lab = 'time'
  if (is.null(y_lab)) y_lab = 'response'

  k <- pc_idx
  fpcs <- paste('fpc', k, sep = '')
  data_temp <- data[, c('ID', group_name, fpcs)]
  classes <- levels(data_temp[, group_name])
  groups <- length(classes)
  scores_mu_g <- unlist(lapply(1:groups, function(x){
    mean(data_temp[data_temp[, group_name] == classes[x], fpcs])
    }))

  plot_data <- data.frame(time_cont * (max(time) - min(time)) + min(time))
  colnames(plot_data) <- 'time'
  for (j in 1:groups){
      plot_data <- cbind(plot_data, as.vector(Mu_functions + FPC_mean[, k] * scores_mu_g[j] *
        sigma_y + mu_y))
  }
  colnames(plot_data) <- c('time', classes)

  plot_melt <- reshape::melt(plot_data, 'time', classes)
  p <- ggplot() +
    geom_line(data=plot_melt, aes(x = time, y = value,
                                  colour = variable,
                                  linetype = variable), lwd = 1) +
    guides(linetype = F) +
    ylim(ymin, ymax) +
    labs(colour=group_name) +
    labs(title= paste(paste('PC', k, sep = ' '),
                      ' (', prop_var_avg[k], ' )', sep=''),
         x = x_lab, y = y_lab) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, size = 15,
                                    face = "bold"),
          axis.text.x = element_text(size = 10, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"))

  #print(p)
  return(results <- list('data' = plot_data,
                         'figure' = p))

  # plot(time_cont * max(time), Mu_functions * sigma_y + mu_y,
  #      type="n", lwd = 2,
  #      xlab='Time', ylab='FPC Scores',
  #      font.lab = 2, cex.lab = 1.2,
  #      ylim = c(ymin, ymax))
  #
  # for (j in 1:groups){
  #   scores_mu_g_temp = mean(data_temp[data_temp[, group_name] == classes[j], fpcs])
  #   lines(time_cont * max(time),
  #         (Mu_functions + FPC_mean[, k] * scores_mu_g_temp) * sigma_y + mu_y,
  #         type = "l", lwd = 3, lty = 1, col = j + 1)
  # }
  # title(main = paste(paste('PC', k, sep = ' '), ' (', prop_var_avg[k], ' )', sep=''))
  # legend('bottomright', classes, lty = rep(1, groups), lwd = rep(3, groups),
  #        col = seq(2, groups + 1), bty = 'n', cex = 0.5)

}
