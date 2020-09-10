#' plot boxplot of model fpc scores by group
#' Suggestion: take out missing values beforehand
#'
#' @param output: The model output list from output_results() function
#' @param group_name One column name of interested group variables in data
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr row_number
#' @import ggpubr
#' @export
plot_fpc_boxplot <- function(output, group_name){
  df <- output$df
  #for (i in 1:length(group_name)) {
  df_temp <- data.frame(df$ID, df$fpc1, df[, group_name])
  colnames(df_temp) <- c('ID', 'fpc', 'var_temp')
  #df_temp <- df_temp[which(df_temp[, 3] != 'Missing: Not provided'), ]
  df_temp[, 3] <-  factor(df_temp[, 3])
  meth = ifelse(length(table(df_temp$var_temp)) > 2, 'anova','t.test')
  #a <- pairwise.t.test(df_temp[, 2], factor(df_temp[, 3]))
  df_tt <- df_temp %>%
    group_by(ID) %>%
    filter(row_number()==1)
  var_tp <- group_name
  colnames(df_tt)[3] <- var_tp
  print(ggboxplot(df_tt, x = var_tp, y = "fpc", color=var_tp, add = "jitter") +
    stat_compare_means(method=meth))
  #}
}
