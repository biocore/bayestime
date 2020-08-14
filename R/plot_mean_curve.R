#' A function to draw diganostic plot
#'
#' @param sfpca_data: The prepared data from prepare_data() function (list)
#' @param sfpca_model: A list of sfpca models with different parameters

plot_mean_curve <- function(sfpca_data, basis, output, id_name, time_name,
                            response_name, variables, ymin=NULL){
  if(!is.null(path)) pdf(path)
  #xpar(mfrow=c(1,2))
  data = sfpca_data$data
  N = sfpca_data$num_subjects
  data = data[, c(id_name, time_name, response_name, variables)]
  colnames(data) = c('ID_unique', 'Time', 'response', variables)
  sigma_y = sd(log(data$response))
  mu_y = mean(log(data$response))
  time_cont <- basis$time_cont
  Y_sparse <- output$Y_sparse
  Mu_functions <- output$Mu_functions
  time_sparse <- output$time_sparse

  if(is.null(ymin) & is.null(ymax)){
    ymin <- floor(min(unlist(Y_sparse)*sigma_y+ mu_y, min(Mu_functions*sigma_y + mu_y))) - 0.1
    ymax <- ceiling(max(unlist(Y_sparse)*sigma_y+ mu_y, max(Mu_functions*sigma_y + mu_y))) + 0.1
  }
  plot(time_cont*(max(data$Time) - min(data$Time)) + min(data$Time), Mu_functions*sigma_y + mu_y, type="l",ylim=c(ymin, ymax),
       xlab='Days of life', ylab='shannon diversity', lwd=5, col=4, font.lab=2, cex.lab=1.2)
  for(i in 1:N){
    lines(time_sparse[[i]]*(max(data$Time) - min(data$Time)) + min(data$Time),Y_sparse[[i]]*sigma_y + mu_y,type="l",lwd=.25)
  }
}
