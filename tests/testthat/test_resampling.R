test_that("CV Initializer stores configuration in env", {
  cv <- SplitCV(folds = 10L)

  expect_equal(environment(cv)$configuration, list(folds = 10L))
})

test_that("CV Instantiator stores configuration", {
  cv <- SplitCV(folds = 10L)
  data <- Dataset(data.frame(x = 1:3, y = 4:6), target = "y", type = "regression", name = "MyDataset")
  cv.data <- cv(data)

  expect_equal(cv.data$configuration, list(folds = 10L))
})

test_that("CV Instantiator has length property", {
  cv <- SplitCV(folds = 10L)
  data <- Dataset(data.frame(x = 1:3, y = 4:6), target = "y", type = "regression", name = "MyDataset")
  cv.data <- cv(data)

  expect_equal(length(cv.data), 10L)
})

test_that("CV Instantiator splits dataset even", {
  cv <- SplitCV(folds = 2L)
  data <- Dataset(data.frame(x = 1:10, y = 10:1), target = "y", type = "regression", name = "MyDataset")
  cv.data <- cv(data)

  expect_equal(length(cv.data), 2L)
  expect_equal(names(cv.data[[1]]), c("train", "validation"))
  expect_equal(length(cv.data[[1]]$train), 5L)
  expect_equal(length(cv.data[[1]]$validation), 5L)
  expect_equal(length(cv.data[[2]]$train), 5L)
  expect_equal(length(cv.data[[2]]$validation), 5L)
})

test_that("CV Instantiator splits dataset uneven", {
  cv <- SplitCV(folds = 3L)
  data <- Dataset(data.frame(x = 1:13, y = 13:1), target = "y", type = "regression", name = "MyDataset")
  cv.data <- cv(data)

  l.train <- sum(sapply(seq(3L), function(i) length(cv.data[[i]]$train)))
  l.validation <- sum(sapply(seq(3L), function(i) length(cv.data[[i]]$validation)))
  expect_equal(l.train, 13L)
  expect_equal(l.validation, 2 * 13L)
})


test_that("Resampling with LM", {
  cv <- SplitCV(folds = 2L)
  data <- Dataset(data.frame(x = 1:10, y = 10:1), target = "y", type = "regression", name = "MyDataset")
  inducer <- ind$lm()

  preds <- resample(data, inducer, cv)
  expect_class(preds, "ResamplePrediction")
  expect_equal(length(preds), 2L)
})
