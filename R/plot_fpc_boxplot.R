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
#' @import ggpubr
#' @import rstatix
#' @export
plot_fpc_boxplot <- function(output, pc_idx, group_name,
                             testing_type = c('parametric', 'non-parametric'),
                             pairwise_testing = FALSE){
  df <- output$df
  pc_name <- paste('fpc', pc_idx, sep = '')
  df_temp <- data.frame(df$ID, df[, pc_name], df[, group_name])
  colnames(df_temp) <- c('ID', 'fpc', 'var_temp')
  df_temp[, 3] <-  factor(df_temp[, 3])

  df_tt <- df_temp %>%
    group_by(ID) %>%
    filter(row_number()==1)

  df_mean <- df_tt %>%
    group_by(var_temp) %>%
    summarise(mean(fpc))
  df_mean <- df_mean[order(df_mean$`mean(fpc)`, decreasing = T), ]
  df_tt$var_temp <- factor(as.character(df_tt$var_temp),
                           levels = c(as.character(df_mean$var_temp)))

  if (testing_type == 'non-parametric'){
    meth = ifelse(length(table(df_temp$var_temp)) > 2, 'kruskal.test','wilcox.test')
    p_global <- stat_compare_means(method=meth, label.y = max(df_tt$fpc)*1.5)
    if (pairwise_testing == TRUE){
      df_test <- df_tt[, -1]
      stat_test <- df_test %>%
        rstatix::pairwise_wilcox_test(
          fpc ~ var_temp
        )
      stat_test <- stat_test[stat_test$p <= 0.05, ]
      stat_test_g1 <- stat_test$group1
      stat_test_g2 <- stat_test$group2
      stat_test_list <- mapply(c, stat_test_g1, stat_test_g2, SIMPLIFY = F)
      p_pairwise <- stat_compare_means(comparisons = stat_test_list,
                                       method='wilcox.test')
    }
  } else {
    meth = ifelse(length(table(df_temp$var_temp)) > 2, 'anova','t.test')
    p_global <- stat_compare_means(method=meth, label.y = max(df_tt$fpc)*1.5)
    if (pairwise_testing == TRUE){
      df_test <- df_tt[, -1]
      stat_test <- df_test %>%
        rstatix::pairwise_t_test(
          fpc ~ var_temp
        )
      stat_test <- stat_test[stat_test$p <= 0.05, ]
      stat_test_g1 <- stat_test$group1
      stat_test_g2 <- stat_test$group2
      stat_test_list <- mapply(c, stat_test_g1, stat_test_g2, SIMPLIFY = F)
      p_pairwise <- stat_compare_means(comparisons = stat_test_list,
                                       method='t.test')
    }
  }


  var_tp <- group_name
  colnames(df_tt) <- c('ID', pc_name, var_tp)
  p <- ggboxplot(df_tt, x = var_tp, y = pc_name, color=var_tp, add = "jitter")
  p <- p + p_global
  if (pairwise_testing == TRUE) p <- p+ p_pairwise
  print(ggpar(p, legend = 'none'))
}
