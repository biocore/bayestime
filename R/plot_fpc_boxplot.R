#' plot boxplot of model fpc scores by group
#' Suggestion: take out missing values beforehand
#'
#' @param output: The model output list from output_results() function
#' @param group_name One column name of interested group variables in data
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr row_number
#' @importFrom dplyr summarise
#' @importFrom magrittr sortLvlsByVar.fnc
#' @import ggpubr
#' @export
plot_fpc_boxplot <- function(output, pc_idx, group_name){
  df <- output$df
  pc_name <- paste('fpc', pc_idx, sep = '')
  df_temp <- data.frame(df$ID, df[, pc_name], df[, group_name])
  colnames(df_temp) <- c('ID', 'fpc', 'var_temp')
  #df_temp <- df_temp[which(df_temp[, 3] != 'Missing: Not provided'), ]
  df_temp[, 3] <-  factor(df_temp[, 3])
  meth = ifelse(length(table(df_temp$var_temp)) > 2, 'anova','t.test')
  df_tt <- df_temp %>%
    group_by(ID) %>%
    filter(row_number()==1)

  df_mean <- df_tt %>%
    group_by(var_temp) %>%
    summarise(mean(fpc))
  df_mean <- df_mean[order(df_mean$`mean(fpc)`, decreasing = T), ]
  df_tt$var_temp <- factor(as.character(df_tt$var_temp),
                        levels = c(as.character(df_mean$var_temp)))
  var_tp <- group_name
  colnames(df_tt) <- c('ID', pc_name, var_tp)
  p <- ggboxplot(df_tt, x = var_tp, y = pc_name, color=var_tp, add = "jitter") +
    stat_compare_means(method=meth)
  print(ggpar(p, legend = 'none'))
  #}
}
