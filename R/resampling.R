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
  cat(sprintf("Configuration: %s", collapse(environment(x)$configuration)))
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
  cat(sprintf("Configuration: %s", collapse(x$configuration)))
  invisible(x)
}

splt <- list()
splt[["cv"]] <- SplitCV
