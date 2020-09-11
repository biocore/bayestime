#' A function to plot fpc curves
#'
#' @param output The model output list from output_results() function
#' @param pc_idx The indices of pc to plot
#' @param ymin The minimum of y lab
#' @param ymax The maximum of y lab
#' @export
plot_fpc_curve <- function(output, pc_idx, ymin = NULL, ymax = NULL){
  time_cont <- output$basis$time_cont
  FPC_mean <- output$FPC_mean
  K <- length(pc_idx)
  pc_names <- unlist(lapply(pc_idx, function(x) paste('PC', x, sep = '')))

  if (is.null(ymin)) ymin <- floor(min((FPC_mean))) - 0.5
  if (is.null(ymax)) ymax <- ceiling(max((FPC_mean))) + 0.5

  plot(time_cont, FPC_mean[, 1], type="n", ylim = c(ymin, ymax),
       xlab = 'time', ylab='FPC Scores', font.lab = 2, cex.lab = 1.2)
  for (k in pc_idx) {
    lines(time_cont, FPC_mean[, k], type = "l",lwd = 3,lty = 1, col = k)
  }
  title(main = 'FPC Curves')
  legend('topright', pc_names, lty = rep(1, K), lwd = rep(3, K),
         col = pc_idx, bty='n', cex = 0.5)
}
