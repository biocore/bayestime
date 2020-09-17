#' A function to plot fpc curves
#'
#' @param output The model output list from output_results() function
#' @param pc_idx The indices of pc to plot
#' @param x_lab Manually set x axis title
#' @param y_lab Manually set y axis title
#' @param ymin The minimum of y lab
#' @param ymax The maximum of y lab
#' @param x_tick Manually set x tick
#' @export
plot_fpc_curve <- function(output, pc_idx, original=FALSE,
                           x_lab = NULL, y_lab = NULL,
                           ymin = NULL, ymax = NULL,
                           x_tick = NULL){
  data <- output$df
  time_cont <- output$basis$time_cont
  FPC_mean <- output$FPC_mean
  K <- ncol(FPC_mean)
  pc_names <- unlist(lapply(1:K, function(x) paste('PC', x, sep = '')))

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
  if (is.null(ymin)) ymin <- floor(min((FPC_mean * sigma_y + mu_y))) - 0.5
  if (is.null(ymax)) ymax <- ceiling(max((FPC_mean * sigma_y + mu_y))) + 0.5
  if (is.null(x_lab)) x_lab = 'time'
  if (is.null(y_lab)) y_lab = 'response'

  plot_data <- data.frame(time_cont * (max(time) - min(time)) + min(time))
  colnames(plot_data) <- 'time'
  for (k in 1:K){
    plot_data[, 1 + k] <- FPC_mean[, k] * sigma_y + mu_y
    colnames(plot_data)[1 + k] <- pc_names[k]
  }

  pc_plot <- unlist(lapply(pc_idx, function(x) paste('PC', x, sep = '')))
  plot_melt <- reshape::melt(plot_data, 'time', pc_plot)
  p <- ggplot() +
    geom_line(data = plot_melt, aes(x = time, y = value, colour = variable,
                                    linetype = variable),
              lwd = 1) +
    guides(linetype=F) +
    ylim(ymin, ymax) +
    labs(colour = 'curve') +
    labs(title = 'FPC Curves',
         x = x_lab, y = y_lab) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.text.x = element_text(size = 10, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"))

  if (!is.null(x_tick)) {
    p <- p + scale_x_continuous(breaks = x_tick)
  }
  print(p)

  return(results <- list('data' = plot_data,
                         'plot' = p))
  # plot(time_cont * (max(time) - min(time)) + min(time),
  #      FPC_mean[, 1] * sigma_y + mu_y, type="n", ylim = c(ymin, ymax),
  #      xlab = x_lab, ylab=y_lab, font.lab = 2, cex.lab = 1.2)
  # for (k in pc_idx) {
  #   lines(time_cont * (max(time) - min(time)) + min(time),
  #         FPC_mean[, k] * sigma_y + mu_y, type = "l",lwd = 3,lty = 1, col = k)
  # }
  # title(main = 'FPC Curves')
  # legend('topright', pc_names, lty = rep(1, K), lwd = rep(3, K),
  #        col = pc_idx, bty='n', cex = 0.5)
}
