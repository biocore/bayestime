#' A function to draw k diganostic plot
#'
#' @param sfpca_data: The prepared data from prepare_data() function (list)
#' @param model: The optimal sfpca model
#' @return A list with the plot
#' @import ggplot2
#' @import bayesplot
#' @export

plot_k_diagnostic <- function(sfpca_data, model){
  loo_best <- model$looic
  pkdf <- data.frame(pk = loo_best$diagnostics$pareto_k,
                     id = unique(sfpca_data$data$ID),
                     xaxis = 1:length(unique(sfpca_data$data$ID)))
  p <- ggplot2::ggplot(pkdf, aes(x = xaxis,y = pk)) +
          geom_text(data=subset(pkdf, pk > 0.7),
                    aes(x = xaxis,y = pk, label = xaxis),
                    vjust = -0.5, size = 3) +
          geom_point(shape = 3, color = "blue") +
          labs(x = "Observation left out", y = "Pareto shape k") +
          geom_hline(yintercept = 0.7, linetype = 2, color = "red", size = 0.2) +
          ggtitle("PSIS-LOO diagnostics") +
          theme_classic() +
          theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
                axis.text.x = element_text(size = 10, face = "bold"),
                axis.text.y = element_text(size = 10, face = "bold"),
                axis.title.x = element_text(size = 12, face = "bold"),
                axis.title.y = element_text(size = 12, face = "bold"))
  print(p)
  bad <- pkdf[pkdf$pk > 0.7, ]
  if (nrow(bad) != 0){
    print(paste('Warning: observation ', bad$xaxis,
                ' (subject ID ', bad$id,
                ') has Pareto k-values greater than 0.7 at ',
                round(bad$pk, 4), sep = ''))
  }
  return(results <- list('figure' = p))
}

#' A function to draw density overlay plot
#'
#' @param sfpca_data: The prepared data from prepare_data() function (list)
#' @param model: The optimal sfpca model
#' @param x_lab: Manually set x axis title
#' @param y_lab: Manually set y axis title
#' @return A list with the plot
#' @import ggplot2
#' @import rstan
#' @import bayesplot
#' @export
plot_posterior_diagnostic <- function(sfpca_data, model,
                                      x_lab = NULL, y_lab = NULL){
  sa <- model$sa
  Nsamples <- model$Nsamples
  Nchains <- model$Nchains
  Ynew <- rstan::extract(sa, "Ynew", permuted = FALSE)
  V <- sfpca_data$visits.vector
  Ynew_transform <- matrix(rep(0, Nsamples / 2 * Nchains * sum(V)),
                           ncol = sum(V))
  ind <- 0
  for (i in 1:(Nsamples / 2)) {
    for (j in 1:Nchains) {
      ind <- ind + 1
      Ynew_transform[ind, ] <- Ynew[i, j, ]
    }
  }
  Ynew_mean <- colMeans(Ynew_transform)
  bayesplot::color_scheme_set("brightblue")
  k <- model$pc
  d <- model$knot
  if (is.null(x_lab)) x_lab = 'standardized response'
  #if (is.null(y_lab)) y_lab = ''
  #plot_data <- data.frame(sfpca_data$data$response, Ynew_transform)
  p <- bayesplot::ppc_dens_overlay(sfpca_data$data$response, Ynew_transform) +
    ggplot2::ggtitle('Posterior Predictive Checking') +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.text.x = element_text(size = 10, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold")) +
    labs(x = x_lab)
  print(p)
  return(results <- list('figure' = p))
}
