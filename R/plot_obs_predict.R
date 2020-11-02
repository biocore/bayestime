#' compare predicted curves (mean with 95% credible intervals) to observed curves
#'
#' @param sfpca_data: The prepared data list from prepare_data() function
#' @param optimal_model: optimal sfpca model
#' @param data: original dataset
#' @param time_name: time variable from original dataset
#' @param response_name: response variable from origina dataset
#' @param unique_subject_id: unique subject id identifier
#' @param subject_selected: interested subject for plotting
#' @param ymin: minimum value of y axis range
#' @param ymax: maximum value of y axis range
#' @param x_lab: label for x axis
#' @param y_lab: label for y axis
#' @export


plot_obs_predict <- function(sfpca_data, optimal_model, data, time_name, response_name, 
                             unique_subject_id, subject_selected, 
                             ymin, ymax, x_lab, y_lab, title='observed vs. predicted'){
    subject_starts = sfpca_data$visits.start
    subject_stops = sfpca_data$visits.stop
    subject_idx = which(unique(sfpca_data$data$ID) == subject_selected)
    
    sigma_y <- sd(data[, response_name], na.rm=T)
    mu_y <- mean(data[, response_name], na.rm=T)
    
    fit_table = rstan::summary(optimal_model$sa, "Ynew")$summary[
                       subject_starts[subject_idx]: subject_stops[subject_idx], c('mean', '2.5%', '97.5%')]
    fit_curve_mean = fit_table[, 'mean']
    fit_curve_q025 = fit_table[, '2.5%']
    fit_curve_q975 = fit_table[, '97.5%'] 
    
    sub_data = data[data[, unique_subject_id] %in%subject_selected,]
    sub_data = sub_data[order(sub_data[, time_name]), ]
    time_obs = sub_data[, time_name]
    response_obs = sub_data[, response_name]
    plot(time_obs, response_obs, type='b', col='black', ylim=c(ymin, ymax), xlab=x_lab, ylab=y_lab, 
         main=title, font.lab=2, cex.lab=1.2)
    lines(time_obs, (fit_curve_mean*sigma_y + mu_y), type='b', col='red')
    lines(time_obs, (fit_curve_q025*sigma_y + mu_y), type='b', col='green')
    lines(time_obs, (fit_curve_q975*sigma_y + mu_y), type='b', col='orange')
    
    legend('bottomright', c('upper95%', 'observed', 'mean', 'lower95%'), 
           col=c('orange', 'black', 'red', 'green'), lty=rep(1,5), bty='n')
    
    p = recordPlot()
    
    return(results <- list('figure' = p, 'time' = time_obs, 'y_obs' = response_obs, 
                            'y_fit_mean' = unname(fit_curve_mean*sigma_y + mu_y),
                            'y_fit_q025' = unname(fit_curve_q025*sigma_y + mu_y),
                            'y_fit_q975' = unname(fit_curve_q975*sigma_y + mu_y)))
             
}