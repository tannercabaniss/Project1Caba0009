#' QQ Plot
#'
#' This function creates a QQ plot of the input data.
#'
#' @param data 1D input vector containing numerical data
#'
#' @return a QQ plot of the input data
#'
#' @export
#' @importFrom stats qnorm
#' @importFrom graphics abline
#'
#' @examples
#' data <- rnorm(1000)
#' QQplot(data)
QQplot <- function(data) {
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
  }

  data_length <- length(data)
  ordered_data <- sort(data)
  prob_levels <- (1:data_length - 0.5) / data_length

  standard_normal_quantiles <- qnorm(prob_levels)

  xlimit_max <- max(max(standard_normal_quantiles), abs(min(standard_normal_quantiles)))
  xlimit_min <- -xlimit_max

  ylimit_max <- max(max(ordered_data), abs(min(ordered_data)))
  ylimit_min <- -ylimit_max

  plot <- plot(standard_normal_quantiles, ordered_data,
               xlab = "Standard Normal Quantiles, q(i)",
               ylab = "Ordered Data, x(i)",
               main = "Q-Q Plot",
               xlim = c(xlimit_min, xlimit_max),
               ylim = c(ylimit_min, ylimit_max))

  plot <- plot + abline(h = 0, v = 0, col = "gray", lty = 2)
  return (plot)
}
