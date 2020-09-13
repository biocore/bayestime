#' A function to plot mean curces
#'
#' @param output: The model output from output_results() function
#' @param original: The option to plot with original or transformed
#' @param xlab: Manually set x axis title
#' @param ylab: Manually set y axis title
#' @param ymin: Manually set minimum of y lab
#' @param ymax: Manually set maximum of y lab
#' @export

plot_mean_curve <- function(output, original = FALSE,
                            xlab=NULL, ylab=NULL,
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
  if (is.null(xlab)) xlab = 'time'
  if (is.null(ylab)) ylab = 'response'

  if (is.null(xlab)) xlab = 'time'
  if (is.null(ylab)) ylab = 'response'
  plot(time_cont * (max(time) - min(time)) + min(time),
       Mu_functions * sigma_y + mu_y,
       ylim = c(ymin, ymax),
       xlab = xlab, ylab = ylab,
       type = "l",lwd = 5, col = 4, font.lab = 2, cex.lab = 1.2)
  for (i in 1:N) {
    lines(time_sparse[[i]] * (max(time) - min(time)) + min(time),
          Y_sparse[[i]] * sigma_y + mu_y,
          type = "l", lwd = .25)
  }
}
