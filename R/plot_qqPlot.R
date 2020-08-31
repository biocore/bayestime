#' A function to generate qqplot of the response variable
#'
#' @param data: A dataframe with the response column to plot
#' @param response: A column name of the interested response in data
#' @import car
#' @export
plot_qqplot <- function(data, response_name){
  if (!response_name %in% colnames(data)) {
    stop("Input response name not in data")
  }
  par(mfrow=c(2,2))
  response <- data[, response_name]
  car::qqPlot(response)
  car::qqPlot(log(response))
  car::qqPlot(sqrt(response))
  #qubic root
  car::qqPlot(response ^ (1/3))
  par(mfrow=c(1,1))
}

#' A function to generate qqplot of the response variable
#'
#' @param output: the outputlist with dataframe 'df'
#' @import car
#' @export
plot_residual <- function(output){
  df <- output$df
  par(mfrow=c(1,2))
  plot(df$residuals)
  car::qqPlot(df$residuals)
  par(mfrow=c(1,1))
}
