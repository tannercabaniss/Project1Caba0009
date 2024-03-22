test_that("Check error catching", {
  expect_error(boxiestCoxiest(matrix(c("-1","2","3","4","5","6","7","8","9"), nrow=3, ncol=3, byrow=TRUE)))
  expect_error(boxiestCoxiest(c(-12,-11,2,3,4,5,76,45,4,4,)))
})
