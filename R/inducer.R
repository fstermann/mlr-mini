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

Param <- function(name, type, range) {
  checkmate::assert_character(name)
  checkmate::assert_choice(type, c("numeric", "integer", "logical", "factor"))
  structure(list(
    name = name,
    type = type,
    range = range
  ), class = "Hyperparameter")
}

p_dbl <- function(lower, upper) {
  checkmate::assert_numeric(lower)
  checkmate::assert_numeric(upper)
  p <- Param(name = "double", type = "numeric", range = c(lower, upper))
  class(p) <- c("DoubleParam", "Param")
  p
}

p_int <- function(lower, upper) {
  checkmate::assert_numeric(lower)
  checkmate::assert_numeric(upper)
  p <- Param(name = "integer", type = "integer", range = c(lower, upper))
  class(p) <- c("IntegerParam", "Param")
  p
}

p_fct <- function(x) {
  p <- Param(name = "factor", type = "factor", range = as.factor(x))
  class(p) <- c("FactorParam", "Param")
  p
}

p_lgl <- function(x) {
  p <- Param(name = "logical", type = "logical", range = c(FALSE, TRUE))
  class(p) <- c("LogicalParam", "Param")
  p
}

hp <- function(...) {
  structure(list(...), class = "HyperparameterSet")
}

print.HyperparameterSet <- function(x, ...) {
  cat("Hyperparameter set:\n")
  for (i in seq_along(x)) {
    cat(
      "  ", x[[i]]$name, " = ", x[[i]]$type, "\n",
      sep = ""
    )
  }
  invisible(x)
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

hyperparameters <- function(x) {
  UseMethod("hyperparameters", x)
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
