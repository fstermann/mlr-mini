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
  expect_equal(checkHyperparameter(list(y = 10.5), hp.space), list(y = FALSE))
  expect_equal(checkHyperparameter(list(x = 0.999, y = 10), hp.space), list(x = TRUE, y = TRUE))
})

test_that("checkHyperparameter Double", {
  hp.space <- hp(x = p_dbl(-10, 10.5))

  expect_equal(checkHyperparameter(list(x = 1), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = -1), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = -10), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = -11), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(x = 10.5), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = 11), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(x = Inf), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(x = -Inf), hp.space), list(x = FALSE))
})

test_that("checkHyperparameter Double Inf", {
  hp.space <- hp(x = p_dbl())

  expect_equal(checkHyperparameter(list(x = 1), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = Inf), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = -Inf), hp.space), list(x = TRUE))
})

test_that("checkHyperparameter Integer", {
  hp.space <- hp(x = p_int(-10, 10))

  expect_equal(checkHyperparameter(list(x = 1), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = -1), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = 1.1), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(x = -1.1), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(x = -10), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = -11), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(x = 10.5), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(x = 11), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(x = Inf), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(x = -Inf), hp.space), list(x = FALSE))
})

test_that("checkHyperparameter Integer Inf", {
  hp.space <- hp(x = p_int())

  expect_equal(checkHyperparameter(list(x = 1), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = Inf), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = -Inf), hp.space), list(x = TRUE))
})

test_that("checkHyperparameter Logical", {
  hp.space <- hp(x = p_lgl())

  expect_equal(checkHyperparameter(list(x = TRUE), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = FALSE), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = 1), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(x = "TRUE"), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(x = "FALSE"), hp.space), list(x = FALSE))
})

invisible(capture.output(test_that("checkHyperparameter Factor", {
  hp.space <- hp(x = p_fct(c("a", "b", "c")))

  expect_equal(checkHyperparameter(list(x = "a"), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = "b"), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = "c"), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = "x"), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(x = "A"), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(x = as.factor("a")), hp.space), list(x = TRUE))
  expect_equal(checkHyperparameter(list(x = as.factor("A")), hp.space), list(x = FALSE))
  expect_equal(checkHyperparameter(list(x = 1), hp.space), list(x = FALSE))
})))

test_that("checkHyperparameter unnamed", {
  hp.space <- hp(x = p_dbl(lower = 0))

  expect_error(checkHyperparameter(list(1), hp.space), "Assertion on 'names\\(hp\\)' failed: Must be of type 'character', not 'NULL'")
})

test_that("checkHyperparameter name unknown", {
  hp.space <- hp(x = p_dbl(lower = 0))

  expect_error(checkHyperparameter(list(unknownX = 1), hp.space), "Assertion on 'names\\(hp\\)' failed: Must be a subset of \\{'x'\\}, but +.* \\{'unknownX'\\}")
})
