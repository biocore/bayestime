#' A function to plot mean curces
#'
#' @param output: The model output from output_results() function
#' @param original: The option to plot with original or transformed
#' @param xlab: Manually set x axis title
#' @param ylab: Manually set y axis title
#' @param ymin: Manually set minimum of y lab
#' @param ymax: Manually set maximum of y lab
#' @return A list with plot
#' @export

plot_mean_curve <- function(output, original = FALSE,
                            x_lab=NULL, y_lab=NULL,
                            ymin=NULL, ymax=NULL){
  data <- output$df
  N <- length(unique(data$ID))

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

  if (is.null(ymin)) {
    ymin <- floor(min(unlist(Y_sparse) * sigma_y + mu_y,
                     Mu_functions * sigma_y + mu_y)) - 0.1 }
  if (is.null(ymax)) {
    ymax <- ceiling(max(unlist(Y_sparse) * sigma_y + mu_y,
                        Mu_functions * sigma_y + mu_y)) + 0.1 }
  if (is.null(x_lab)) x_lab = 'time'
  if (is.null(y_lab)) y_lab = 'response'

  p <- ggplot() +
    geom_line(aes(x = time_cont * (max(time) - min(time)) + min(time),
                  y = Mu_functions * sigma_y + mu_y, group = 1),
        #      color='blue',
              color = 'black',
              lwd = 2) +
    ylim(ymin, ymax) +
    labs(title= 'Mean Curve',
         x = x_lab, y = y_lab) +
    ggtitle('Population mean curve') +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.text.x = element_text(size = 10, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"))


  for (i in 1:N){
    if (length(time_sparse[[i]]) == 1) {
      print(paste('object', i, 'has only 1 observation'))
      next
    }
    p <- p + geom_line(aes_string(x = time_sparse[[i]] *
                                    (max(time) - min(time)) + min(time),
                                  y = Y_sparse[[i]] * sigma_y + mu_y),
                       lwd = 0.1, color=i, group=1)
  }
  # if (!is.null(x_tick)) {
  #   p <- p + scale_x_continuous(breaks = x_tick)
  # }
  print(p)
  return(results <- list('plot' = p))
  # plot(time_cont * (max(time) - min(time)) + min(time),
  #      Mu_functions * sigma_y + mu_y,
  #      ylim = c(ymin, ymax),
  #      xlab = x_lab, ylab = y_lab,
  #      type = "l",lwd = 5, col = 4, font.lab = 2, cex.lab = 1.2)
  # for (i in 1:N) {
  #   lines(time_sparse[[i]] * (max(time) - min(time)) + min(time),
  #         Y_sparse[[i]] * sigma_y + mu_y,
  #         type = "l", lwd = .25)
  # }
}
