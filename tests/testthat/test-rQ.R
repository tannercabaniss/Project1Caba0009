test_that("Check error catching", {
  expect_error(rQ(list(c(-2,1,-1,0))))
  expect_error(rQ(c("one", "two", "three")))
  expect_error(rQ(matrix(c(1,2,3,4,5,6), nrow=3, ncol=2, byrow=TRUE)))
})

test_that("rQ calculation", {
  data <- rnorm(1000)
  ordered_data <- sort(data)
  prob_levels <- (1:length(data) - 0.5) / length(data)
  std_norm_quan <- qnorm(prob_levels)
  basedrQ <- cor(ordered_data, std_norm_quan)

  expect_equal(round(basedrQ, 5), round(rQ(data), 5))
})
