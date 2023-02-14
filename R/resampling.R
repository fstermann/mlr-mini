#' @import checkmate
Split <- function(configuration, subclass, instantiator) {
  checkmate::assertList(configuration, names = "unique")
  checkmate::assertString(subclass)
  checkmate::assertFunction(instantiator)

  SplitInstantiator <- function(dataset, ...) {
    structure(
      instantiator(dataset, ...),
      class = paste0("Split", c(subclass, ""), "Instance")
    )
  }

  environment(SplitInstantiator) <- list2env(
    list(configuration = configuration),
    parent = environment(SplitInstantiator),
  )
  environment(instantiator) <- environment(SplitInstantiator)
  structure(SplitInstantiator, class = paste0("Split", c(subclass, "")))
}

#' @title 'SplitCV' class generator
#'
#' @param folds Number of folds to generate
#'
#' @export
SplitCV <- function(folds = 5L) {
  Split(
    configuration = list(
      folds = folds
    ),
    subclass = "CV",
    instantiator = function(dataset) {
      list(
        configuration = configuration,
        dataset.name = dataset$name,
        indices = sample(nrow(dataset))
      )
    }
  )
}

#' @title Length method for 'SplitCVInstance' class
#'
#' @param x An object of class 'SplitCVInstance'
#'
#' @export
length.SplitCVInstance <- function(x) {
  x$configuration$folds
}

#' @title Access fold i of 'SplitCVInstance' class
#'
#' @param x An object of class 'SplitCVInstance'
#' @param i The index of the fold to access
#'
#' @export
`[[.SplitCVInstance` <- function(x, i) {
  checkmate::assert_integerish(i, lower = 1, upper = x$configuration$folds)
  is.train <- cut(seq(1, length(x$indices)), breaks = x$configuration$folds, labels = FALSE) == i
  list(
    train = x$indices[is.train],
    validation = x$indices[!is.train]
  )
}

as.list.SplitInstance <- function(x) {
  lapply(
    seq(length(x)), function(i) x[[i]]
  )
}

# TODO: Move this to a utils file
collapse <- function(x) {
  out <- paste0(names(x), " = ", x)
  out <- paste(out, collapse = ", ")
  out
}

#' @title Print method for 'Split' class
#'
#' @param x An object of class 'Split'
#' @param ... Other arguments passed to 'print'
#'
#' @export
print.Split <- function(x, ...) {
  cat(sprintf("%s Instantiator\n", class(x)[1]))
  cat(sprintf("Configuration: %s\n", collapse(environment(x)$configuration)))
  invisible(x)
}

#' @title Print method for 'SplitInstance' class
#'
#' @param x An object of class 'SplitInstance'
#' @param ... Other arguments passed to 'print'
#'
#' @export
print.SplitInstance <- function(x, ...) {
  cat(
    sprintf(
      "%s of the '%s' dataset (%i rows)\n",
      class(x)[1], x$dataset.name, length(x$indices)
    )
  )
  cat(sprintf("Configuration: %s\n", collapse(x$configuration)))
  invisible(x)
}

splt <- list()
splt[["cv"]] <- SplitCV

#' @title Resampler function
#'
#' @param dataset The Dataset object to resample
#' @param inducer The Inducer Model to use for prediction
#' @param splitClass The Split class to use
#'
#' @export
resample <- function(dataset, inducer, splitClass) {
  assertClass(dataset, "Dataset")
  assertClass(inducer, "Inducer")
  assertClass(splitClass, "Split")

  data.split <- splitClass(dataset)
  predictions <- lapply(
    as.list(data.split),
    function(x) predict(train(inducer, dataset[x$train, ]), newdata = dataset[x$validation, ]$env$data)
  )
  structure(predictions, class = "ResamplePrediction")
}
