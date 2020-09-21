#' plot boxplot of model fpc scores by group
#' Suggestion: take out missing values beforehand
#'
#' @param output The model output list from output_results() function
#' @param pc_idx The pc index number
#' @param group_name One column name of interested group variables in data
#' @param x_lab Manually set x axis title
#' @param y_lab Manually set y axis title
#' @param p_title Manually set plot title
#' @param testing_type The option to set (non-)parametric test
#' @param pairwise_testing The option to set pairwise test
#' @param pval_show_all The option to show all pvals or just the significant
#' @param global_testing The option to set global test
#' @param group_order The string vector for boxplot order
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr row_number
#' @importFrom dplyr summarise
#' @import ggpubr
#' @import rstatix
#' @export
plot_fpc_boxplot <- function(output, pc_idx, group_name,
                             x_lab = NULL, y_lab = NULL, p_title = NULL,
                            # ymin = NULL, ymax = NULL,
                             testing_type = c('parametric', 'non-parametric'),
                             pairwise_testing = FALSE,
                             pval_show_all = FALSE,
                             global_testing = FALSE,
                             p_adjust_meth = "none",
                             group_order = NULL){
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
  if (is.null(group_order)) {
     df_tt$var_temp <- factor(as.character(df_tt$var_temp),
                           levels = c(as.character(df_mean$var_temp)))
  } else {
    df_tt$var_temp <- factor(as.character(df_tt$var_temp),
                             levels = group_order)
  }


  if (testing_type == 'non-parametric') {
    pairwise_meth <- 'wilcox.test'
    meth = ifelse(length(table(df_temp$var_temp)) > 2,
                  'kruskal.test', 'wilcox.test')
    p_global <- stat_compare_means(method = meth,
                                   label.y = max(df_tt$fpc) * 1.5)
    if (pairwise_testing == TRUE) {
      df_test <- df_tt[, -1]
      stat_test <- df_test %>%
        rstatix::pairwise_wilcox_test(
          fpc ~ var_temp,
          p.adjust.method = p_adjust_meth
          )
    }
  } else {
    pairwise_meth <- 't.test'
    meth = ifelse(length(table(df_temp$var_temp)) > 2, 'anova','t.test')
    p_global <- stat_compare_means(method = meth,
                                   label.y = max(df_tt$fpc) * 1.5)
    if (pairwise_testing == TRUE){
      df_test <- df_tt[, -1]
      stat_test <- df_test %>%
        rstatix::pairwise_t_test(
          fpc ~ var_temp,
          p.adjust.method = p_adjust_meth
          )
    }
  }
  if (pairwise_testing == TRUE){
    if (p_adjust_meth == 'none') {
      if (pval_show_all == FALSE){
        stat_test <- stat_test[stat_test$p <= 0.05, ]
      }
    } else {
      if (pval_show_all == FALSE){
        stat_test <- stat_test[stat_test$p.adj <= 0.05, ]
      }
    }
    stat_test_g1 <- stat_test$group1
    stat_test_g2 <- stat_test$group2
    stat_test_list <- mapply(c, stat_test_g1, stat_test_g2, SIMPLIFY = F)
    stat_test <- stat_test %>% add_xy_position(x = 'var_temp')
    if (p_adjust_meth == 'none') {
     # p_pairwise <- stat_compare_means(comparisons = stat_test_list,
      #                                 method = pairwise_meth)

      p_pairwise <- stat_pvalue_manual(stat_test, label = 'p',
                                       step.increase = 0.05)
    } else {
      #stat_test <- stat_test %>% add_xy_position(x = 'var_temp')
      p_pairwise <- stat_pvalue_manual(stat_test, label = 'p.adj',
                                       step.increase = 0.05)
    }
  }


  # p_pairwise <- stat_compare_means(comparisons = stat_test_list,
  #                                  method = 't.test')


  # p_pairwise <- stat_compare_means(comparisons = stat_test_list,
  #                                  method = 't.test')

  if (is.null(x_lab)) x_lab = group_name
  if (is.null(y_lab)) y_lab = paste(pc_name, 'scores')
  # if (is.null(ymin)) ymin <- min(df_tt[, 2])
  # if (is.null(ymax)) ymax <- max(df_tt[, 2])
  var_tp <- group_name
  colnames(df_tt) <- c('ID', pc_name, var_tp)
  p <- ggboxplot(df_tt, x = var_tp, y = pc_name,
                 color = var_tp, add = "jitter",
                 xlab = x_lab, ylab = y_lab) +
   # ylim(ymin, ymax) +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"),
          axis.text.x = element_text(size = 10, face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"),
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"))+
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
  if (global_testing == TRUE) p <- p + p_global
  if (pairwise_testing == TRUE) p <- p+ p_pairwise
  if (!is.null(p_title)) p <- p + ggtitle(p_title)
  p <- ggpar(p, legend = 'none')
  print(p)
  return(results <- list('data' = df_tt,
                         'figure' = p))
}
