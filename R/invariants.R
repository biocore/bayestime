#' generate sfpca models with different parameters
#'
#' @param data: A data frame with subject id, time and variables
#' @param unique_subject_id: A column name corresponding to unique
#' subject id in the data (string)
#' @param time_name: A column name corresponding to time variable
#' @param var_name: A vector of interested variable name, default is null
#' @return A vector of column names in data that do not change with
#' time variable
#' @importFrom tidyr spread
#' @export
#' @examples
#' data("ECAM")
#' invariants(ECAM, 'studyid', 'month_of_life')
invariants <- function(data, unique_subject_id, time_name,
                       var_name = NULL){
  if (!is.null(var_name)) {
    data <- data[, c(unique_subject_id, time_name, var_name)]
  } else {
    var_name <- colnames(data)
  }

  var_invariant <- c()
  for (i in 1:length(var_name)) {
    var_temp <- var_name[i]

    #skip id, time and constant variable
    if (var_temp == unique_subject_id) next
    if (var_temp == time_name) next
    if (length(unique(data[,var_temp])) == 1) next

    #get wide format data
    data_temp <- data[, c(unique_subject_id, time_name, var_temp)]
    data_wide <- tidyr::spread(data_temp, time_name, var_temp)
    #count number of subject with same value
    count <- 0
    for (k in 1:nrow(data_wide)) {
      vec_temp <- na.omit(unlist(data_wide[k, ])[-1])
      count <- ifelse(length(unique(vec_temp)) == 1, count + 1, count)
    }
    if (count == nrow(data_wide)) var_invariant <- c(var_invariant, var_temp)
  }
  return(var_invariant)
}
