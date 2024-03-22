#' Bivariate Normality Check
#'
#' This function evaluates a bivariate data set for normality by observing the predicted and actual probabilities of
#' containment within a predetermined contour of constant density.
#'
#' @param data 2D input matrix or data frame containing bivariate numerical data
#' @param prob The probability used in determining the chi-sq value for degrees of freedom = 2.
#'
#' @return named list with plot of data and the actual vs predicted percentages of data within ellipse
#' @export
#' @importFrom stats cov qchisq
#'
#' @examples
#' vec1 <- rnorm(100)
#' vec2 <- rnorm(100)
#' data <- data.frame(x=vec1, y=vec2)
#' biNormCheck(data, prob=0.5)
biNormCheck <- function(data, prob=0.5) {
  data <- as.matrix(data)
  if (dim(data)[2] != 2) {
    rlang::abort("The data provided is not bivariate")
    return(NULL)
  }

  if (!is.numeric(data[,1]) || !is.numeric(data[,2])) {
    rlang::abort("The data provided is non-numeric")
    return(NULL)
  }

  deg_free <- 2
  chi_sq_value <- qchisq(p = prob, df = deg_free)

  xbar_vec <- colMeans(data)
  x_minus_xbar <- data - xbar_vec
  cov_mat <- cov(data)
  inv_cov_mat <- MASS::ginv(cov_mat)

  total_observations <- dim(data)[1]

  observations_inside <- 0
  for (i in 1:total_observations) {
    x_minus_xbar_i <- x_minus_xbar[i,]
    if ((t(x_minus_xbar_i) %*% inv_cov_mat %*% x_minus_xbar_i) <= chi_sq_value) {
      observations_inside <- observations_inside + 1
    }
  }

  observed_percentage <- observations_inside / total_observations

  eigen_decomp <- eigen(cov_mat)

  lambda <- eigen_decomp$values
  e <- eigen_decomp$vectors

  center <- xbar_vec
  v1 <- e[,1]
  v2 <- e[,2]
  a <- sqrt(lambda[1])
  b <- sqrt(lambda[2])

  theta <- seq(0, 2 * pi, length.out = 100)

  scaling_factors <- sqrt(chi_sq_value * lambda)

  theta <- seq(0, 2 * pi, length.out = 100)

  x <- center[1] + scaling_factors[1] * cos(theta) * v1[1] + scaling_factors[2] * sin(theta) * v2[1]
  y <- center[2] + scaling_factors[1] * cos(theta) * v1[2] + scaling_factors[2] * sin(theta) * v2[2]

  xlimit_max <- max(max(data[,1]), abs(min(data[,1]))) + 0.5
  xlimit_min <- -xlimit_max

  ylimit_max <- max(max(data[,2]), abs(min(data[,2]))) + 0.5
  ylimit_min <- -ylimit_max

  ellipse_df <- data.frame(x = x, y = y)
  data_df <- as.data.frame(data)
  plot <- ggplot2::ggplot(ellipse_df, ggplot2::aes(x, y)) +
            ggplot2::geom_point(data = data_df, ggplot2::aes(x = data_df[,1], y = data_df[,2]), color = "red") +
            ggplot2::geom_polygon(fill = "blue", color = "black", alpha = 0.25) +
            ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "solid") +
            ggplot2::geom_vline(xintercept = 0, color = "black", linetype = "solid") +
            ggplot2::xlim(xlimit_min, xlimit_max) +
            ggplot2::ylim(ylimit_min, ylimit_max) +
            ggplot2::xlab("X") +
            ggplot2::ylab("Y")

  result_list <- list(estimated_probability = prob, actual_probability = observed_percentage, plot = plot)
  return (result_list)
}
