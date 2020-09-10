#' A function to generate plots of a group of variables
#' Suggestion: take out missing values beforehand
#'
#' @param data: A dataframe with the response column to plot
#' @param time_name: the column name corresponding to the time variable in the data (string)
#' @param response_name: the column name of the intersted response variable (string)
#' @param unique_subject_id the column name corresponding to unique subject id in the data (string)
#' @param variable_name One column name of interested variable from dataset
#' @import ggplot2
#' @export
plot_group <- function(data, time_name, response_name, unique_subject_id, variable_name){
  group <- data[, c(time_name, response_name, unique_subject_id, variable_name)]
  group[, variable_name] <- as.factor(group[, variable_name])
 # for(i in 1:length(variable_name)){
  group <- group[which(group[,variable_name] != 'Missing: Not provided'), ]
  time <- as.numeric(as.character(group[, time_name]))
  response <- group[, response_name]
  unique_id <- group[, unique_subject_id]
  print(ggplot(group, aes(x = time, y = response, group = unique_id,
                          color = group[, variable_name])) +
          labs(color = variable_name) +
          theme_bw() +
          theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
          geom_line(alpha=0.2) +
          geom_smooth(se = F, size = 2, aes(group = group[, variable_name]),
                      level=0.95) +
          xlab(time_name) + ylab(response_name) +
          theme(axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold"),
                legend.position = "top"))
}
