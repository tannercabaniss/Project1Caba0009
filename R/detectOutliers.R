#' Detecting Outliers
#'
#' This function checks a multivariate numerical data set for outliers by utilizing dot plots, scatter plots,
#'     standardized values, and chi-square plots.
#'
#' @param data 2D or greater input matrix or data frame containing multivariate numerical data
#'
#' @return a dotplot for each varibles, a scatter plot for each pair of variables, standardized values, and a chi-square plot
#' @export
#' @importFrom grid unit
#' @importFrom stats cov var
#'
#' @examples
#' vec1 <- rnorm(50)
#' vec2 <- rnorm(50)
#' vec3 <- rnorm(50)
#' vec4 <- rnorm(50)
#' vec5 <- rnorm(50)
#' vec6 <- rnorm(50)
#' vec7 <- rnorm(50)
#' vec8 <- rnorm(50)
#' vec9 <- rnorm(50)
#' data <- data.frame(V1=vec1, V2=vec2, V3=vec3, V4=vec4, V5=vec5, V6=vec6, V7=vec7, V8=vec8, V9=vec9)
#' detectOutliers(data)
detectOutliers <- function(data) {
  data <- as.data.frame(data)


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

  dot_plots <- list()
  for (i in 1:dim(data)[2]) {
    binwidth <- (1/30) * (abs(range(data[,i])[1]) + abs(range(data[,i])[2]))
    dotsize <- min(100 / dim(data)[1], 1)
    p <- ggplot2::ggplot(data, ggplot2::aes_string(x = colnames(data)[i])) +
      ggplot2::geom_dotplot(binwidth = binwidth, dotsize = dotsize) +
      ggplot2::labs(title = paste(colnames(data)[i])) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    dot_plots[[i]] <- p
  }

  scatter_plots <- list()
  for (i in 1:dim(data)[2]) {
    for (j in 1:dim(data)[2]) {
      p <- ggplot2::ggplot(data, ggplot2::aes_string(x = colnames(data)[i], y = colnames(data)[j])) +
        ggplot2::geom_point() +
        ggplot2::labs(title = paste(colnames(data)[i], "and", colnames(data)[j])) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

      scatter_plots[[length(scatter_plots) + 1]] <- p
    }
  }

  ncols_dot <- floor(sqrt(dim(data)[2]))
  nrows_dot <- ceiling(dim(data)[2] / ncols_dot)

  ncols_scatter <- floor(sqrt(length(scatter_plots)))
  nrows_scatter <- ceiling(length(scatter_plots) / ncols_scatter)

  dot_plots_arranged <- gridExtra::grid.arrange(
    grobs = dot_plots,
    ncol = ncols_dot,
    nrow = nrows_dot,
    widths = rep(1/ncols_dot, ncols_dot),
    heights = rep(1, nrows_dot),
    padding = unit(0.01, "line")
  )

  scatter_plots_arranged <- gridExtra::grid.arrange(
    grobs = scatter_plots,
    ncol = ncols_scatter,
    nrow = nrows_scatter,
    widths = rep(1/ncols_scatter, ncols_scatter),
    heights = rep(1, nrows_scatter),
    padding = unit(0.01, "line")
  )

  variances <- apply(data, 2, var)
  xbar <- matrix(colMeans(data), ncol=1)
  z_mat <- matrix(NA, nrow=nrow(data), ncol=ncol(data))

  for (j in 1:ncol(data)) {
    z_mat[,j] <- round((data[,j] - xbar[j]) / sqrt(variances[j]), 4)
  }

  colnames(z_mat) <- colnames(data)

  count_outliers <- function(x) {
    sum(x > 3 | x < -3)
  }

  outlier_counts <- apply(z_mat, 2, count_outliers)

  cat("Number of outliers (z values > 3 or < -3) for each column:\n")
  print(outlier_counts)

  chi_sq_plot <- mvNormCheck(data)
  print(chi_sq_plot)
}
