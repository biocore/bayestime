#' Function to perform rda analysis on fpc score from model
#'
#' @param output: The output list from output_results() function
#' @param model: A set of variable names insterested in original data
#' @param variables: A list of interested variables for rda analysis
#' @return A list of results with plot and rda table
#' @import vegan
#' @import ggplot2
#' @export
effectSize_rda <- function(output, model, variables, trace = FALSE){
  variables <- variables[!variables %in% 'ID']
  df <- output$df
  df <- df[complete.cases(df), ]
  dat <- df[, variables]

  pc.names <- numeric(model$pc)
  pc.names <- sapply(1:model$pc, function(i){
    paste('fpc', i, sep = '')
  })
  pc <- df[, pc.names]

  mod0 <- vegan::rda(pc ~ 1., dat)  # Model with intercept only
  mod1 <- vegan::rda(pc ~ ., dat)  # Model with all explanatory variables
  set.seed(111)
  step.res <- vegan::ordiR2step(mod0, mod1, perm.max = 1000, trace = trace)

  #add effect-size
  table <- step.res$anova
  if (is.null(table)) return(print('no non-redundant variable'))
  table.row <- nrow(table)
  R2.adj <- c(table$R2.adj[1])
  for (i in 1:(table.row - 1)) {
    R2.adj <- c(R2.adj, table$R2.adj[i + 1] - table$R2.adj[i])
  }
  table$ES.RDA <- R2.adj
  table <- table[-table.row, ]
  if (trace == TRUE) print(step.res$call)
  rownames(table) <- gsub('[+] ', '', rownames(table))
  covariates <- rownames(table)
  p <- ggplot2::ggplot(table, aes(x=reorder(covariates, ES.RDA), y=ES.RDA,
                                  fill=covariates)) +
    labs(x = 'Non-redundant Covariants', y = 'Effect Size') +
    geom_bar(stat='identity') +
    theme_classic() +
    theme(axis.text = element_text(size = 10),
          axis.title = element_text(size = 14,face = "bold"),
          legend.position = "none") +
    coord_flip()
  print(p)
  return(results <- list('data' = table,
                         'figure' = p))
}
