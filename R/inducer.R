Inducer <- function(name, configuration, hyperparameters) {
  checkmate::assert_character(name)
  checkmate::assert_list(configuration)
  checkmate::assert_list(hyperparameters)

  structure(list(
    name = name,
    configuration = configuration,
    hyperparameters = hyperparameters
  ), class = "Inducer")
}

#' @export
print.Inducer <- function(x, ...) {
  cat(
    "Inducer: ", x$name, "\n",
    "Configuration: ", collapse(x$configuration), "\n\n",
    sep = ""
  )
  invisible(x)
}

collapse <- function(x) {
  out <- paste0(names(x), " = ", x)
  out <- paste(out, collapse = ", ")
  out
}

train <- function(x, ...) {
  UseMethod("train", x)
}

configuration <- function(x) {
  UseMethod("configuration", x)
}

configuration.Inducer <- function(x) {
  print(x$configuration)
}

`configuration<-` <- function(x, value) {
  x$configuration <- value
}

hyperparameters.Inducer <- function(x) {
  print(x$hyperparameters)
}

#' @export
InducerLinearModel <- function(...) {
  inducer <- Inducer(
    name = "Linear Model",
    configuration = list(..., verbose = 0),
    hyperparameters = list(
      Param(name = "tol", type = "numeric", range = c(0, 1)),
      Param(name = "singular.ok", type = "numeric", range = c(0, 1))
    )
  )
  class(inducer) <- c("InducerLinearModel", "Inducer")
  inducer
}

#' @export
train.InducerLinearModel <- function(inducer, dataset, weights = NULL, ...) {
  f <- get_formula(dataset)
  if (is.null(weights)) {
    model <- stats::lm(f, data = dataset$env$data)
  } else {
    model <- stats::lm(f, weights = weights, data = dataset$env$data)
  }
  ModelLinearModel(inducer = inducer, dataset = dataset, model = model)
}

Model <- function(inducer, dataset, model) {
  structure(
    list(inducer = inducer, dataset = dataset, model = model),
    class = "Model"
  )
}

ModelRegression <- function(inducer, dataset, model) {
  structure(
    list(inducer = inducer, dataset = dataset, model = model),
    class = c("ModelRegression", "Model")
  )
}

ModelLinearModel <- function(inducer, dataset, model) {
  structure(
    list(inducer, dataset = dataset, model = model),
    class = c("ModelLinearModel", "ModelRegression", "Model")
  )
}

#' @export
predict.ModelLinearModel <- function(model, newdata, ...) {
  unname(predict(model$model, newdata = newdata, ...))
}

get_formula <- function(dataset) {
  checkmate::assert_class(dataset, "Dataset")
  f <- as.formula(
    paste0(dataset$target, " ~ ", paste(dataset$features, collapse = " + "))
  )
  f
}

#' @export
InducerXgboost <- function(...) {
  inducer <- Inducer(
    name = "XGBoost",
    configuration = list(..., verbose = 0),
    hyperparameters = list(
      Param("eta", "numeric", range = c(0.01, 0.2)),
    )
  )
  class(inducer) <- c("InducerXgboost", "Inducer")
  inducer
}

ind <- list()
ind[["lm"]] <- InducerLinearModel
ind[["xgboost"]] <- InducerXgboost
