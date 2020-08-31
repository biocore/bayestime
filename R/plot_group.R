#' A function to generate plots of a group of variables
#' Suggestion: take out missing values beforehand
#'
#' @param data: A dataframe with the response column to plot
#' @param time_name: the column name corresponding to the time variable in the data (string)
#' @param response_name: the column name of the intersted response variable (string)
#' @param unique_subject_id the column name corresponding to unique subject id in the data (string)
#' @param group_name A vector with column names of interested group variables in data
#' @import ggplot2
#' @export
plot_group <- function(data, time_name, response_name, unique_subject_id, group_name){
  group <- data[, c(time_name, response_name, unique_subject_id, group_name)]
  group[, group_name] <- as.factor(group[, group_name])
  for(i in 1:length(group_name)){
    group <- group[which(group[,group_name[i]] != 'Missing: Not provided'), ]
    time <- as.numeric(as.character(group[, time_name]))
    response <- group[, response_name]
    unique_id <- group[, unique_subject_id]
    vars <- group_name[i]
    print(ggplot(group, aes(x = time, y = response, group = unique_id,
                            color = group[,vars])) +
            geom_line(alpha=0.2) +
            geom_smooth(se = F, size = 2, aes(group = group[, group_name[i]]),
                        level=0.95) +
            xlab(time_name) + ylab(response_name) + ggtitle(group_name[i]) +
            theme(axis.title.x = element_text(face = "bold"),
                  axis.title.y = element_text(face = "bold"),
                  legend.position = "top"))
  }
}
