#' Boxiest Coxiest
#'
#' This function performs the Box Cox transformation on a data frame or data matrix and returns the augmented data.
#'
#' @param data the input data matrix or data frame containing univariate or multivariate data. All input data must be positive
#'
#' @return the augmented data following Box Cox transformation, and a figure showing the respective lambda and likelihood function relations
#' @export
#' @importFrom stats optimize quantile
#'
#' @examples
#' vec1 <- rexp(100, rate=2.0)
#' vec2 <- rexp(100, rate=0.5)
#' vec3 <- rexp(100, rate=0.1)
#' data <- data.frame(V1 = vec1, V2 = vec2, V3=vec3)
#' boxiestCoxiest(data)
boxiestCoxiest <- function(data) {
  if (!all(apply(data, 2, function(x) all(is.numeric(x))))) {
    rlang::abort("The data provided is not numeric")
  }

  if (!all(data >0)) {
    rlang::abort("The data provided is not all positive")
  }

  x_lambda_4_34 <- function(data_column, lambda) {
    augmented_data <- numeric(length(data_column))
    for (i in seq_along(data_column)) {
      if (lambda != 0) {
        augmented_data[i] <- ((data_column[i]^lambda) - 1)/(lambda)
      }
      else if (lambda == 0) {
        augmented_data[i] <- log(data_column[i])
      }
    }
    return(augmented_data)
  }

  xbar_lambda_4_36 <- function(data_column, lambda) {
    x_lambdas <- x_lambda_4_34(data_column, lambda)
    xbar_lambda <- sum(x_lambdas)/length(x_lambdas)
    return(xbar_lambda)
  }

  likelihood_4_35 <- function(lambda, data_column) {
    x_lambdas <- x_lambda_4_34(data_column, lambda)
    xbar_lambda <- xbar_lambda_4_36(data_column, lambda)
    n <- length(x_lambdas)
    likelihood_func <- ((-n/2) * log((1/n) * sum((x_lambdas - xbar_lambda)^2))) + (lambda - 1) * sum(log(data_column))
    return(likelihood_func)
  }

  data <- as.data.frame(data)
  column_names <- colnames(data)

  augmented_data <- data.frame(matrix(ncol = ncol(data), nrow = nrow(data)))
  colnames(augmented_data) <- paste0(colnames(data), "_aug")

  for (col_name in column_names) {
    data_column <- data[[col_name]]
    maximum <- optimize(f = function(lambda) likelihood_4_35(lambda, data_column),
                        interval = c(-5, 5), maximum = TRUE)
    augmented_data[[paste0(col_name, "_aug")]] <- x_lambda_4_34(data_column, maximum$maximum)
    print(paste("Optimal Lambda for", col_name, ":", round(maximum$maximum, 3)))
  }

  likelihood_data <- data.frame(matrix(ncol = length(column_names), nrow = 1000))
  colnames(likelihood_data) <- paste0(column_names, "_l(lambda)")
  lambda_values <- seq(-5, 5, length.out = 1000)

  for (col_name in column_names) {
    data_column <- data[[col_name]]
    likelihood_values <- sapply(lambda_values, function(lambda) likelihood_4_35(lambda, data_column))
    new_col_name <- paste0(col_name, "_l(lambda)")
    likelihood_data[[new_col_name]] <- likelihood_values
  }



  lambda_plots <- list()
  for (i in 1:ncol(likelihood_data)) {
    col_name <- colnames(likelihood_data)[i]

    max_index <- which.max(likelihood_data[[i]])
    optimal_lambda <- lambda_values[max_index]

    percentile_05 <- quantile(likelihood_data[, i], probs = 0.05)

    p <- ggplot2::ggplot(likelihood_data, ggplot2::aes(x = lambda_values, y = likelihood_data[,i])) +
      ggplot2::geom_line() +
      ggplot2::geom_vline(xintercept = optimal_lambda, linetype = "dashed", color = "red") +
      ggplot2::annotate("text", x = optimal_lambda - 0.125, y = percentile_05,
                        label = paste("Optimal value of lambda =", round(optimal_lambda, 3)),
                        color = "black", angle = 90, hjust = -0.2) +
      ggplot2::labs(title = col_name, x = "lambda", y = "l(lambda)") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    lambda_plots[[i]] <- p
  }

  # Arrange plots in a grid
  ncols_lambda <- floor(sqrt(ncol(likelihood_data)))
  nrows_lambda <- ceiling(ncol(likelihood_data) / ncols_lambda)

  lambda_plots_arranged <- gridExtra::grid.arrange(
    grobs = lambda_plots,
    ncol = ncols_lambda,
    nrow = nrows_lambda,
    widths = rep(1/ncols_lambda, ncols_lambda),
    heights = rep(1, nrows_lambda),
    padding = unit(0.01, "line")
  )

  return(augmented_data)

}
