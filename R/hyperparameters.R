Hyperparameter <- function(name, type, range) {
  checkmate::assert_character(name)
  checkmate::assert_choice(type, c("numeric", "integer", "logical", "factor"))
  structure(list(
    name = name,
    type = type,
    range = range
  ), class = "Hyperparameter")
}

#' @title Double Hyperparameter
#' 
#' @param lower Lower bound of the hyperparameter. Defaults to -Inf.
#' @param upper Upper bound of the hyperparameter. Defaults to Inf.
#' 
#' @import checkmate
#' @export
p_dbl <- function(lower=-Inf, upper=Inf) {
  checkmate::assert_numeric(lower)
  checkmate::assert_numeric(upper)
  p <- Hyperparameter(name = "double", type = "numeric", range = c(lower, upper))
  class(p) <- c("DoubleHyperparameter", class(p))
  p
}

#' @title Integer Hyperparameter
#' 
#' @param lower Lower bound of the hyperparameter. Defaults to -Inf.
#' @param upper Upper bound of the hyperparameter. Defaults to Inf.
#' 
#' @import checkmate
#' @export
p_int <- function(lower=-Inf, upper=Inf) {
  checkmate::assert_numeric(lower)
  checkmate::assert_numeric(upper)
  p <- Hyperparameter(name = "integer", type = "integer", range = c(lower, upper))
  class(p) <- c("IntegerHyperparameter", class(p))
  p
}

#' @title Factor Hyperparameter
#' 
#' @param x Factor values.
#' 
#' @import checkmate
#' @export
p_fct <- function(x) {
  p <- Hyperparameter(name = "factor", type = "factor", range = as.factor(x))
  class(p) <- c("FactorHyperparameter", class(p))
  p
}

#' @title Logical/Boolean Hyperparameter
#' 
#' @param x Logical value. Defaults to TRUE.
#' 
#' @import checkmate
#' @export
p_lgl <- function(x = TRUE) {
  p <- Hyperparameter(name = "logical", type = "logical", range = c(FALSE, TRUE))
  class(p) <- c("LogicalHyperparameter", class(p))
  p
}

HyperparameterSpace <- function(...) {
  structure(list(...), class = "HyperparameterSpace")
}
hp <- HyperparameterSpace

print.HyperparameterSpace <- function(x, ...) {
  cat("Hyperparameter set:\n")
  for (i in seq_along(x)) {
    cat(
      "  ", x[[i]]$name, " = ", x[[i]]$type, "\n",
      sep = ""
    )
  }
  invisible(x)
}

checkHyperparameter = function(hp, hp.space) {
  checkmate::assertClass(hp.space, "HyperparameterSpace")
  
  for (hp.name in names(hp)) {
    p = hp[[hp.name]]
    p.space = hp.space[[hp.name]]
    valid = assertValidHP(p, p.space)
    print(sprintf("Parameter %s is valid: %s", hp.name, valid))
  }
}

assertValidHP = function(x, value, ...) UseMethod("assertValidHP")
assertValidHP.DoubleHyperparameter = function(x, value, ...) {
  checkmate::assert_numeric(value)
  x$range[0] <= value && value <= x$range[0]
}
assertValidHP.IntegerHyperparameter = function(x, value, ...) {
  checkmate::assert_numeric(value)
  x$range[0] <= value && value <= x$range[0]
}
assertValidHP.FactorHyperparameter = function(x, value, ...) {
  value %in% x$range
}
assertValidHP.LogicalHyperparameter = function(x, value, ...) {
  checkmate::assertLogical(value)
  value %in% x$range # Probably not needed, since it would be a constant with length(range()) == 1
}



hyperparameters <- function(x) {
  UseMethod("hyperparameters", x)
}


# hyperparameter <- function(type, range) {
#   data <- list(type = type, range = range)
#   structure(data, class = "Hyperparameter")
# }
#
# hp_range <- function(values) {
#   structure(list(values = values),
#     class = "HpRange"
#   )
# }
#
#
#
# p_dbl <- function(x_min = -Inf, x_max = Inf) {
#   checkmate::assertDouble(x_min, upper = x_max)
#   checkmate::assertDouble(x_max, lower = x_min)
#
#   range <- hp_range(values = c(x_min, x_max))
#   hyperparameter(type = "dbl", range = range)
# }
#
# p_int <- function(x_min = -Inf, x_max = Inf) {
#   checkmate::assertInt(x_min, upper = x_max)
#   checkmate::assertInt(x_max, lower = x_min)
#
#   range <- hp_range(values = c(x_min, x_max))
#   hyperparameter(type = "int", range = range)
# }
#
# p_fct <- function(...) {
#   range <- hp_range(values = as.factor(c(...)))
#   hyperparameter(type = "fct", range = range)
# }
#
#
# hp <- function(...) {
#   hps <- list(...)
#
#   structure(hps, class = "hp")
# }
#
#
# print.hp <- function(x, ...) {
#   # hp.names = names(x)
#
#   dt <- cbind(name = names(x), data.table::rbindlist(x))
#
#   # Format hyperparameter ranges
#   hp.ranges <- sapply(dt$range, function(x) paste(x, collapse = ", "))
#   hp.format = ifelse(dt$type == "fct", "{%s}", "[%s]")
#   dt$range <- mapply(sprintf, hp.format, hp.ranges)
#
#   print(dt)
#   invisible(x)
# }
#
#
# hp(x = p_dbl(0, 1), y = p_int(-2, 2), z = p_fct("a", "b", "c"))
#
#
#
#
# checkHyperparameter <- function(hp.params, hp.config) {
#   TRUE
# }
