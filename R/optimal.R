#' A function to compare sfpca model restuls and return the optimal one
#'
#' @param model_list: A list of sfpca models with different parameters
#' @return The sfpca model with the optimal result
#' @export
optimal <- function(model_list){
  len <- length(model_list)
  looic.list <- lapply(1:len, function(i) model_list[[i]]$looic)
  looic.obj <- loo::loo_compare(looic.list)
  looic.obj.df <- as.data.frame(looic.obj)[, c('elpd_diff', 'se_diff')]

  model.name <- rownames(looic.obj)
  model.index <- as.numeric(gsub(".*?([0-9]+).*", "\\1", model.name))

  pc <- unlist(lapply(model.index, function(x) model_list[[x]]$pc))
  knot <- unlist(lapply(model.index, function(x) model_list[[x]]$knot))
  looic.obj.df$pc <- pc
  looic.obj.df$knot <- knot
  print(looic.obj.df)

  optimal.name <- rownames(looic.obj)[1]
  optimal.index <- as.numeric(gsub(".*?([0-9]+).*", "\\1", optimal.name))
  return(optimal.index)
}
