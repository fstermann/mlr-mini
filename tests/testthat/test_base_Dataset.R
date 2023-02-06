#' @import checkmate
test_that("Dataset throws an error if target not present in DataFrame", {
  badcalldf = data.frame(x = 1:3, y = c(TRUE, FALSE, TRUE))
  expect_error(Dataset(badcalldf, target = "z", type = "classification"),
               'Data does not contain target Column: "z"')
})
test_that("Dataset throws when trying to remove target column", {
  data <- data.frame(x = 1:10, y = 1:10)
  dataset <- Dataset(data, type = "classification", target = "y")
  expect_error(dataset[c(1,2), "x"],
               sprintf('Cannot remove target column "y"'))
})
