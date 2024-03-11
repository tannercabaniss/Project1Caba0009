#' Check Univariate Normality
#'
#' This function will check a 1 dimensional dataset for normality. Please note that this function utilizes
#' an empiral test and is not as accurate as functions like shapiro.test() which provides a more formal assessment of
#' the data.
#'
#' @param data 1D input vector or list containing data
#'
#' @return True if data is normal and False if data is not normal
#' @export
#' @importFrom stats sd
#' @examples
#' data <- rnorm(1000)
#' uniNormCheck(data)
uniNormCheck <- function (data) {
  if (!is.numeric(data)) {
    rlang::abort("Input data is non numerical")
    return (NULL)
  }

  if (!(length(data) > 1)) {
    rlang::abort("Input data must have length greater than 1")
  }

  if ((is.vector(data) && length(dim(data)) == 0) || (is.list(data) && any(!sapply(data, is.list)))){
    data_length <- length(data)
    data_mean <- sum(data) / data_length
    data_std <- sd(data)

    xbar_1s <- c(data_mean - data_std, data_mean + data_std)
    xbar_2s <- c(data_mean - 2*data_std, data_mean + 2*data_std)

    u_1 <- 0
    for (i in 1:data_length) {
      if (data[i] > xbar_1s[1] && data[i] < xbar_1s[2]) {
        u_1 <- u_1 + 1
      }
    }

    phat_1 <- u_1 / data_length

    u_2 <- 0
    for (j in 1:data_length) {
      if (data[j] > xbar_2s[1] && data[j] < xbar_2s[2]) {
        u_2 <- u_2 + 1
      }
    }

    phat_2 <- u_2 / data_length

    if ((abs(phat_1 - 0.683) > (1.396 / sqrt(data_length))) || (abs(phat_2 - 0.954) > (0.628 / sqrt(data_length)))){
      return (FALSE)
    }
    else {
      return (TRUE)
    }
  }
  else {
    rlang::abort("Input is not a 1 dimensional vector or list")
    return (NULL)
  }
}
