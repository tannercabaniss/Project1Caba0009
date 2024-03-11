test_that("Normality check works (TRUE case)", {
  expect_equal(uniNormCheck(rnorm(1000)), TRUE)
})

test_that("Normality check works (FALSE case)", {
  expect_equal(uniNormCheck(rexp(1000, rate = 2)), FALSE)
})

test_that("Check error catching", {
  expect_error(uniNormCheck(matrix(c(1,2,3,4,5,6), nrow=3, ncol=2, byrow=TRUE)))
  expect_error(uniNormCheck(c("one", "two", "three", "four")))
  expect_error(uniNormCheck(0))
})
