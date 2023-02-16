test_that("Hyperparameter has correct class", {
  expect_equal(class(p_dbl()), c("DoubleHyperparameter", "Hyperparameter"))
  expect_equal(class(p_int()), c("IntegerHyperparameter", "Hyperparameter"))
  expect_equal(class(p_fct(c("a"))), c("FactorHyperparameter", "Hyperparameter"))
  expect_equal(class(p_lgl()), c("LogicalHyperparameter", "Hyperparameter"))
})

test_that("Double Hyperparameter Range", {
  hp <- p_dbl(0.0, 1.0)

  expect_equal(length(hp$range), 2)
  expect_equal(hp$range[1], 0)
  expect_equal(hp$range[2], 1)
  expect_output(print(hp$range), "\\[0, 1\\]")
})

test_that("Integer Hyperparameter Range", {
  hp <- p_int(0, 10)

  expect_equal(length(hp$range), 2)
  expect_equal(hp$range[1], 0)
  expect_equal(hp$range[2], 10)
  expect_output(print(hp$range), "\\[0, 10\\]")
})

test_that("Integer Hyperparameter Range invalid type double", {
  expect_error(p_int(lower = 0.5), "Assertion on 'lower' failed: Must be of type 'integerish'")
  expect_error(p_int(upper = 0.5), "Assertion on 'upper' failed: Must be of type 'integerish'")
})


test_that("Factor Hyperparameter Range", {
  hp <- p_fct(c("a", "b", "c"))

  expect_equal(length(hp$range), 3)
  expect_equal(levels(hp$range), c("a", "b", "c"))
  expect_output(print(hp$range), "\\{'a', 'b', 'c'\\} \\(3 levels\\)")
})

test_that("Logical Hyperparameter Range", {
  hp <- p_lgl()

  expect_equal(length(hp$range), 2)
  expect_equal(hp$range[1], FALSE)
  expect_equal(hp$range[2], TRUE)
  expect_output(print(hp$range), "\\[FALSE, TRUE\\]")
})
