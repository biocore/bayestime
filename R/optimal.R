#' A function to compare sfpca model restuls and return the optimal one
#'
#' @param model_list: A list of sfpca models with different parameters
#' @return One sfpca model with the optimal result
optimal <- function(model_list){
  len <- length(model_list)
  looic.list <- lapply(1:len, function(i) model_list[[i]]$looic)
  looic.obj <- loo::loo_compare(looic.list)
  print(looic.obj)
  model.name <- rownames(looic.obj)[1]
  model.index <- as.numeric(gsub(".*?([0-9]+).*", "\\1", model.name))
  return(model_list[[model.index]])
}
