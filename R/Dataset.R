#' @title Creation class: "Dataset"
#' 
#' @param data Data, either a data.table or data.frame
#' @param target Target column
#' @param type Type of the task. One of c("regression", "classification")
#' @param name Name of the dataset
#' 
#' @import checkmate
#' @export
Dataset <- function(data, target, type = c("regression", "classification"),
                    name = deparse(substitute(data), 20)) {
  assertString(type)
  assertString(name)
  assertDataFrame(data)
  assertString(target)
  if(!target %in% colnames(data)) {
    stop(sprintf("Data does not contain target Column: %s", deparse(target)))
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

#' @export
print.Dataset <- function(x, ...) {
  cat(sprintf("Dataset %s, predicting %s (%s)\n", deparse(x$name), deparse(x$target), x$type))
  print(x$env$data)
  invisible(x)
}

#' @export
`[.Dataset` <- function(x, i, j, inplace=FALSE, ...) {
  if (!missing(j)) {
    if(!x$target %in% j)
      stop("Cannot remove target column ", deparse(x$target))
  }
  
  if (inplace == FALSE) {
    return(Dataset(data=x$env$data[i, j],
            target=x$target,
            type = x$type,
            name = x$name))
  }
  
  dimensions <- dim(x$env$data)
  x$env$data <- x$env$data[i, j]
  difference <- dimensions - dim(x$env$data)
  cat(sprintf("note: %s rows and %s columns have been deleted \n", difference[[1]], difference[[2]]))
  invisible(x)
}

#' @export
metainfo.Dataset <- function(x, ...) {
  types <- vapply(x$env$data, class, character(1))
  structure(list(features = types[names(types) == x$features],
                 target = types[names(types) == x$target],
                 nrow = nrow(x$env$data),
                 type = x$type,
                 missings = sum(is.na(x$env$data))),
            class = "DatasetInfo")
}

#' The Number of Rows of an Array
#' 
#' @param x The array/object to return rows for
#' 
#' @export
nrow <- function(x) {
  UseMethod("nrow")
}

#' The Number of Rows of an Array
#' 
#' @param x The array/object to return rows for
#' 
#' @export
nrow.default <- function(x) {
  base::nrow(x)
}

#' The Number of Rows of a `Dataset` object
#' 
#' Uses `base::nrow` under the hood
#' 
#' @param x The Dataset
#' 
#' @export
nrow.Dataset <- function(x) {
  base::nrow(x$env$data)
}
