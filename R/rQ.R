#' Correlation Coefficient (rQ)
#'
#' This function determines the rQ value for a dataset to aid in the determination of normality.
#'
#' @param data 1D input vector containing numerical data
#'
#' @return the rQ value (correlation coefficient) of the ordered data and standard normal quantiles
#' @export
#'
#' @examples
#' data <- rnorm(1000)
#' QQplot(data)
rQ <- function(data) {
  if (!is.vector(data)) {
    rlang::abort("Input data is not a vector")
    return (NULL)
  }

  if (!is.numeric(data)) {
    rlang::abort("Input data is non numerical")
    return (NULL)
  }

  if (length(dim(data)) != 0) {
    rlang::abort("Input vector is not one dimensional")
    return (NULL)
  }

  data_length <- length(data)
  data_mean <- mean(data)
  ordered_data <- sort(data)
  prob_levels <- (1:data_length - 0.5) / data_length
  std_norm_quan <- qnorm(prob_levels)
  std_norm_quan_mean <- mean(std_norm_quan)

  numerator <- 0
  for (i in 1:data_length) {
    numerator <- numerator + ((ordered_data[i] - data_mean)*(std_norm_quan[i] - std_norm_quan_mean))
  }

  temp_den_sum_1 <- 0
  temp_den_sum_2 <- 0
  for (i in 1:data_length) {
    temp_den_sum_1 <- temp_den_sum_1 + (ordered_data[i] - data_mean)^2
    temp_den_sum_2 <- temp_den_sum_2 + (std_norm_quan[i] - std_norm_quan_mean)^2
  }
  denominator <- sqrt(temp_den_sum_1) * sqrt(temp_den_sum_2)

  rQ_result <- numerator / denominator
  return (rQ_result)
}
