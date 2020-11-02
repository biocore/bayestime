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
                             x_lab, y_lab, x_axis_ticks, x_axis_range, y_axis_ticks, y_axis_range,
                             title='observed vs. predicted'){
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
    y_fit_mean = unname(fit_curve_mean*sigma_y + mu_y)
    y_fit_q025 = unname(fit_curve_q025*sigma_y + mu_y)
    y_fit_q975 = unname(fit_curve_q975*sigma_y + mu_y)
    
    table_obs_predict = data.frame(cbind(rep(time_obs, 4), rep(c('observed', 'predicted_mean', 'predicted_q025',    
                                                                 'predicted_q975'), each=length(time_obs)),
                                         c(response_obs, y_fit_mean, y_fit_q025, y_fit_q975)))
    colnames(table_obs_predict) = c('time', 'type', 'response')
    #table_obs_predict$time = as.numeric(levels(table_obs_predict$time))[table_obs_predict$time] 
    #table_obs_predict$response = as.numeric(levels(table_obs_predict$response))[table_obs_predict$response] 
    table_obs_predict$time = as.numeric(table_obs_predict$time)
    table_obs_predict$response = as.numeric(table_obs_predict$response) 
    
    p <- ggplot(aes(x = time, y = response, colour = type, group=type), data = table_obs_predict) +  
          geom_point() + geom_line() +
        labs(title = title, x = x_lab, y = y_lab) + 
        scale_x_continuous(limits=x_axis_range, breaks = x_axis_ticks) + 
        scale_y_continuous(limits=y_axis_range, breaks = y_axis_ticks)+
        theme_classic() +
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        legend.title = element_blank(), legend.position = 'top')   
    
    return(results <- list('figure' = p, 'data' = table_obs_predict))
             
}