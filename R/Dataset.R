# Creation class: "Dataset"
#' @import checkmate
Dataset <- function(data, target, type = c("regression", "classification"),
                    name = deparse(substitute(data), 20)) {
  assertString(type)
  assertString(name)
  assertDataFrame(data)
  assertString(target)
  if(!target %in% colnames(data)) {
    stop(sprintf("Data does not contain target Column: %s", deparse(target)))
  }
  if(type == "classification") {
    stopifnot(class(data$target == "factor"))
  }
  env <- new.env(parent = emptyenv())
  env$data <- data.table::as.data.table(data)
  result <- list(name = name,
                 type = type,
                 features = colnames(data)[which(colnames(data) != target)],
                 target = target,
                 env = env)
  structure(result, class = "Dataset")
}

print.Dataset <- function(x, ...) {
  cat(sprintf("Dataset %s, predicting %s (%s)\n", deparse(x$name), deparse(x$target), x$type))
  print(x$env$data)
  invisible(x)
}

`[.Dataset` <- function(x, i, inplace = FALSE, ...) {
  if (!missing(..1)) {
    if(!x$target %in% ...)
      stop("Cannot remove target column ", deparse(x$target))
  }
  dimensions <- dim(x$env$data)
  if (inplace == FALSE) {
    return(x$env$data[i, ...])
  }
  x$env$data <- x$env$data[i, ...]
  difference <- dimensions - dim(x$env$data)
  cat(sprintf("note: %s rows and %s columns have been deleted \n", difference[[1]], difference[[2]]))
  invisible(x)
}

metainfo.Dataset <- function(x, ...) {
  types <- vapply(x$env$data, class, character(1))
  structure(list(features = types[names(types) == x$features],
                 target = types[names(types) == x$target],
                 nrow = nrow(x$env$data),
                 type = x$type,
                 missings = sum(is.na(x$env$data))),
            class = "DatasetInfo")
}