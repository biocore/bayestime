#' A function to plot fpc curves
#'
#' @param output The model output list from output_results() function
#' @param pc_idx The indices of pc to plot
#' @param xlab: Manually set x axis title
#' @param ylab: Manually set y axis title
#' @param ymin The minimum of y lab
#' @param ymax The maximum of y lab
#' @export
plot_fpc_curve <- function(output, pc_idx, original=FALSE,
                           xlab = NULL, ylab = NULL,
                           ymin = NULL, ymax = NULL){
  data <- output$df
  time_cont <- output$basis$time_cont
  FPC_mean <- output$FPC_mean
  K <- length(pc_idx)
  pc_names <- unlist(lapply(pc_idx, function(x) paste('PC', x, sep = '')))

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
  if (is.null(ymin)) ymin <- floor(min((FPC_mean * sigma_y))) - 0.5
  if (is.null(ymax)) ymax <- ceiling(max((FPC_mean * sigma_y))) + 0.5
  if (is.null(xlab)) xlab = 'time'
  if (is.null(ylab)) ylab = 'response'

  plot(time_cont * (max(time) - min(time)) + min(time),
       FPC_mean[, 1] * sigma_y, type="n", ylim = c(ymin, ymax),
       xlab = xlab, ylab=ylab, font.lab = 2, cex.lab = 1.2)
  for (k in pc_idx) {
    lines(time_cont * (max(time) - min(time)) + min(time),
          FPC_mean[, k] * sigma_y, type = "l",lwd = 3,lty = 1, col = k)
  }
  title(main = 'FPC Curves')
  legend('topright', pc_names, lty = rep(1, K), lwd = rep(3, K),
         col = pc_idx, bty='n', cex = 0.5)
}
