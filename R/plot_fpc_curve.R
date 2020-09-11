#' A function to plot fpc curves
#'
#' @param output The model output list from output_results() function
#' @param original The option to plot with original or transformed
#' time and response value
#' @export
plot_fpc_curve <- function(output, original = FALSE, ymin = NULL, ymax = NULL){
  time_cont <- output$basis$time_cont
  FPC_mean <- output$FPC_mean
  K <- output$rotation$npcs
  pc_names <- unlist(lapply(1:K, function(x) paste('PC', x, sep = '')))

  if (is.null(ymin) & is.null(ymax)) {
    ymin <- floor(min((FPC_mean))) - 0.5
    ymax <- ceiling(max((FPC_mean))) + 0.5
  }

  plot(time_cont, FPC_mean[, 1], type="n", ylim = c(ymin, ymax),
       xlab = 'time', ylab='FPC Scores', font.lab = 2, cex.lab = 1.2)
  for (k in 1:K) {
    lines(time_cont, FPC_mean[, k], type = "l",lwd = 3,lty = 1, col = k + 1)
  }
  title(main = 'FPC Curves')
  legend('topright', pc_names, lty = rep(1, K), lwd = rep(3, K),
         col = seq(2, K + 1, 1), bty='n', cex = 0.5)
}
