#' Multivariate Normality Check
#'
#' This function checks a multivariate numerical data set for normality by utilizing the squared generalized distances
#' and their relationship to specific chi-square values.
#'
#' @param data 2D or greater input matrix or data frame containing multivariate numerical data
#'
#' @return a chi-square plot
#' @export
#' @importFrom stats cov
#'
#' @examples
#' vec1 <- rnorm(100)
#' vec2 <- rnorm(100)
#' vec3 <- rnorm(100)
#' data <- data.frame(x=vec1, y=vec2, z=vec3)
#' mvNormCheck(data)
mvNormCheck <- function(data) {
  data <- as.matrix(data)

  if (!dim(data)[2] > 1) {
    rlang::abort("The data provided is not multivariate")
    return(NULL)
  }

  for (i in 1:dim(data)[2]) {
    if (!is.numeric(data[,i])) {
      rlang::abort("The data provided is non-numeric")
      return(NULL)
    }
  }

  xbar <- colMeans(data)
  x_minus_xbar <- sweep(data, 2, xbar, "-")
  cov_data <- cov(data)
  inv_cov_data <- solve(cov_data)
  sq_gen_dist <- vector(mode="numeric", length=dim(data)[1])

  for (i in 1:dim(data)[1]) {
    x_minus_xbar_i <- x_minus_xbar[i,]
    sq_gen_dist[i] <- t(x_minus_xbar_i) %*% inv_cov_data %*% x_minus_xbar_i
  }
  sorted_sq_gen_dist <- sort(sq_gen_dist)

  deg_free <- dim(data)[2]
  n <- dim(data)[1]
  quantiles <- vector(mode="numeric", length=dim(data)[1])
  for (i in 1:length(quantiles)) {
    quantiles[i] <- qchisq((n - i + 0.5) / n, df = deg_free)
  }
  sorted_quantiles <- sort(quantiles)

  xlimit_max <- max(sorted_quantiles) + 0.5
  xlimit_min <- 0

  ylimit_max <- max(sorted_sq_gen_dist) + 0.5
  ylimit_min <- 0

  plot <- plot(sorted_quantiles, sorted_sq_gen_dist,
               xlab = "Quantiles",
               ylab = "Squared Generalized Distance",
               main = "Chi-Square Plot",
               xlim = c(xlimit_min, xlimit_max),
               ylim = c(ylimit_min, ylimit_max))
  plot <- plot + abline(h = 0, v = 0, col = "gray", lty = 2) + abline(a = 0, b = 1, col = "red")

  return (plot)
}
