test_that("Check error catching", {
  expect_error(QQplot(list(c(-2,1,-1,0))))
  expect_error(QQplot(c("one", "two", "three")))
  expect_error(uniNormCheck(matrix(c(1,2,3,4,5,6), nrow=3, ncol=2, byrow=TRUE)))
})
