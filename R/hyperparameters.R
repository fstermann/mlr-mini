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


hyperparameters <- function(x) {
  UseMethod("hyperparameters", x)
}



