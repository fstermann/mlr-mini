test_that("HP implementation", {
  expect_equal(hp, HyperparameterSpace)
})

test_that("HP with names", {
  hp.space <- hp(alpha = p_dbl(lower = 0), beta = p_int(0, 100), gamma = p_lgl())

  expect_equal(length(hp.space), 3)
  expect_equal(names(hp.space), c("alpha", "beta", "gamma"))
})

test_that("HP with duplicate names", {
  hp.space <- hp(alpha = p_dbl(lower = 0), beta = p_int(0, 100), beta = p_lgl())

  expect_equal(length(hp.space), 3)
  expect_equal(names(hp.space), c("alpha", "beta", "beta.1"))
})

test_that("HP without names", {
  hp.space <- hp(p_dbl(lower = 0), p_int(0, 100), p_lgl(), p_lgl())

  expect_equal(length(hp.space), 4)
  expect_equal(names(hp.space), c("x", "y", "z", "a"))
})


test_that("checkHyperparameter", {
  hp.space <- hp(x = p_dbl(lower = 0), y = p_int(0, 100))

  expect_equal(checkHyperparameter(list(x = 1), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = -1), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(y = 10), hp.space), list(y = TRUE))
})
