#' prepare data for sfpca model
#'
#' @param data target longitudinal data for analysis (must be a data frame)
#' @param unique_subject_id the column name corresponding to unique subject id in the data (string)
#' @param time_name the column name corresponding to the time variable in the data (string)
#' @param response_name the column name of the intersted response variable (string)
#' @param transform_y the ways of transforming the response variable: standardize/center/NULL
#' @param scale_time the option of whether or not to scale the time variable to be within [0, 1] (True/False)
#' @param group_name the column name of group id, set to NA if no input
#' @param average the option to average duplicate response values
#' @return A list containing the prepared data for sfpca model
#' @importFrom dplyr distinct
#' @export
#' @examples
#' data("ECAM")
#' ECAM <- ECAM[!duplicated(ECAM[, c('studyid', 'month_of_life')]), ]
#' dat <- prepare_data(ECAM, unique_subject_id = 'studyid', time_name = 'month_of_life',
#'                    response_name = 'shannon', transform_y='standardize', scale_time=T)
prepare_data = function(data, unique_subject_id, time_name, response_name,
                        transform_y = NULL, scale_time = FALSE,
                        group_name = NULL, average = FALSE){

  if (!(unique_subject_id %in% colnames(data)) |
      !(time_name %in% colnames(data)) |
      !(response_name %in% colnames(data))) stop("Variable name is not in data")

  # data_remove <- data[!duplicated(data[, c(unique_subject_id, ttime_name)]), ]
  # keys <- c(unique_subject_id, time)
  # X <- data.table::as.data.table(data)
  # data_mean <- X[, list(response_mean = mean(shannon)),keys]
  # check if each subject has unique measurement at each time point
  # data_check <- data[, c(as.character(unique_subject_id), as.character(time_name))]
  # if (sum(duplicated(data_check)) != 0) {
  #   dupli_ids <- unique(data_check[, unique_subject_id])
  #   stop(paste('Subject', paste(dupli_ids, sep = ',', collapse = ','), 'has duplicate measurements.', by= ' '))
  # }

  ID.list = unique(data[, unique_subject_id])
  data[, 'response_mean']= data[, response_name]
  for(i in 1:length(ID.list)){
    subject_i = data[data[, unique_subject_id] == ID.list[i], ]
    for (t in subject_i[, time_name]){
      subject_i_t = subject_i[subject_i[, time_name] %in% t, ]
      if (dim(subject_i_t)[1] > 1){
        print(paste('replicated values of subject', i, 'at time', t, sep='_'))
        if (average == FALSE) {
          return()
        } else {
          data[data[, unique_subject_id] == ID.list[i] &
                 data[, time_name] %in% t, 'response_mean'] =
            mean(subject_i_t[, response_name], na.rm=T)
        }
      }
    }
  }
   data <- dplyr::distinct(data, !!as.name(unique_subject_id),
                               !!as.name(time_name), .keep_all=TRUE)
   response_name <- 'response_mean'

  # create new ID
  data$ID <- as.character(data[, unique_subject_id])
  N <- length(unique(data$ID)) # total number of unique subjects

  #convert time and response to numeric
  # data[, c(time_name, response_name)] <- lapply(data[, c(time_name,
  #                                                        response_name)],
  #                                               function(x) as.numeric(as.character(x)))

  # convert group id to be numeric
  if (!is.null(group_name)) {
    data[, group_name] <- as.numeric(as.factor(data[, group_name]))
  }

  # create time
  # keep original time
  if (scale_time == TRUE){
    data$time_ori <- data[, time_name]
    data$time <- (data[, time_name] - min(data[, time_name])) /
      (max(data[, time_name]) - min(data[, time_name]))
  } else{
    data$time_ori <- data[, time_name]
    data$time <- data[, time_name]
  }

  T_len <- length(unique(data$time)) # total number of sampling time points

  # transform response (code updated on 08/16/2019)
  # keep original response
  if (!is.null(transform_y)) {
    if (transform_y == 'standardize'){
      data$response_ori <- data[, response_name]
      data$response <- (data[, response_name] - mean(data[, response_name], na.rm=T)) /
        (sd(data[, response_name], na.rm=T))
    } else if (transform_y == 'center'){
      data$response_ori <- data[, response_name]
      data$response <- (data[, response_name] - mean(data[, response_name], na.rm=T))
    }
  } else {
    data$response_ori <- data[, response_name]
    data$response <- data[, response_name]
  }


  # re-order the data by ID and time
  data <- data[order(data$ID, data$time), ]

  # create visits vector, response and time matrix
  ID.list <- unique(data$ID)
  visits.vector <- vector(mode = "numeric", length = N)
  response.list <- NULL
  time.matrix <- matrix(rep(0, N * T_len), nrow = N)

  # visits index for each individual when stacking the data
  visits.stop <- vector(mode = "numeric", length = N)

  # size index for each individual in covariance matrix
  cov.start <- cov.stop <- vector(mode = "numeric", length = N)

  # group id based on interested group
  id_group <- vector(mode = "numeric", length = N)

  for(i in 1:N){
    # visits vector
    subject_i <- data[data$ID == ID.list[i], ]
    subject_i$n_visits <- dim(subject_i)[1]
    visits.vector[i] <- unique(subject_i$n_visits)

    # visits index
    visits.stop[i] <- sum(visits.vector)

    # covariance size index
    cov.stop[i] <- sum(visits.vector ^ 2)

    # response matrix
    # response.matrix[i, ] = c(subject_i$response, rep(0, T - unique(subject_i$n_visits)))
    response.list <- c(response.list, subject_i$response)

    # time matrix
    time.matrix[i, ] <- c(subject_i$time, rep(0, T_len - unique(subject_i$n_visits)))

    # group id based on interested group
    if (!is.null(group_name)) {
      id_group[i] <- unique(subject_i[, group_name])
    }
    rm(subject_i)
  }
  visits.start <- c(1, visits.stop[-N] + 1)
  cov.start <- c(1, cov.stop[-N] + 1)
  cov.size <- sum(visits.vector ^ 2)

  sfpca_data = list(data=data, num_subjects=N, num_times=T_len,
                    response.list=response.list, time.matrix=time.matrix,
                    visits.vector=visits.vector, visits.start=visits.start,
                    visits.stop=visits.stop, cov.start=cov.start, cov.stop=cov.stop,
                    cov.size=cov.size, id_group=id_group)
  return(sfpca_data)
}
