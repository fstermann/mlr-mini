Hyperparameter <- function(type, range) {
  checkmate::assert_choice(type, c("numeric", "integer", "logical", "factor"))

  class(range) <- "Range"
  structure(
    list(
      type = type,
      range = range
    ),
    class = "Hyperparameter"
  )
}

#' @export
format.Range <- function(x, ...) paste(x, collapse = ", ")

#' @export
format.NumericRange <- function(x, ...) sprintf("[%s, %s]", x[1], x[2])

#' @export
format.FactorRange <- function(x, max.display = 3, ...) {
  len <- length(levels(x))
  if (is.logical(max.display) && max.display) max.display <- len

  x.cut <- levels(x)[seq(min(max.display, len))]
  x.preview <- paste0("'", paste(x.cut, collapse = "', '"), "'")
  sprintf(
    "{%s%s} (%s level%s)",
    x.preview,
    if (len > max.display) ", ..." else "",
    len,
    if (len != 1) "s" else ""
  )
}

#' @export
print.Range <- function(x, ...) {
  cat(format(x))
  invisible(x)
}

#' @export
print.Hyperparameter <- function(x, ...) {
  cat(sprintf("Hyperparamter\n  Type:  %s\n  Range: %s", x$type, format(x$range)))
  invisible(x)
}

#' Double Hyperparameter
#'
#' Generate a Hyperparameter of type `Double`
#'
#' @param lower Lower bound of the hyperparameter. Defaults to -Inf.
#' @param upper Upper bound of the hyperparameter. Defaults to Inf.
#'
#' @import checkmate
#' @export
p_dbl <- function(lower = -Inf, upper = Inf) {
  checkmate::assert_numeric(lower)
  checkmate::assert_numeric(upper)
  p <- Hyperparameter(type = "numeric", range = c(lower, upper))
  class(p) <- c("DoubleHyperparameter", class(p))
  class(p$range) <- c("NumericRange", class(p$range))
  p
}

#' Integer Hyperparameter
#'
#' Generate a Hyperparameter of type `Integer`
#'
#' @param lower Lower bound of the hyperparameter. Defaults to -Inf.
#' @param upper Upper bound of the hyperparameter. Defaults to Inf.
#'
#' @import checkmate
#' @export
p_int <- function(lower = -Inf, upper = Inf) {
  if (!is.infinite(lower)) checkmate::assertIntegerish(lower, upper = upper)
  if (!is.infinite(upper)) checkmate::assertIntegerish(upper, lower = lower)
  p <- Hyperparameter(type = "integer", range = c(lower, upper))
  class(p) <- c("IntegerHyperparameter", class(p))
  class(p$range) <- c("NumericRange", class(p$range))
  p
}

#' Factor Hyperparameter
#'
#' Generate a Hyperparameter of type `Factor`
#'
#' @param x Factor values.
#'
#' @import checkmate
#' @export
p_fct <- function(x) {
  p <- Hyperparameter(type = "factor", range = as.factor(x))
  class(p) <- c("FactorHyperparameter", class(p))
  class(p$range) <- c("FactorRange", class(p$range))
  p
}

#' Logical/Boolean Hyperparameter
#'
#' Generate a Hyperparameter of type `Logical`
#'
#' @import checkmate
#' @export
p_lgl <- function() {
  p <- Hyperparameter(type = "logical", range = c(FALSE, TRUE))
  class(p) <- c("LogicalHyperparameter", class(p))
  class(p$range) <- c("NumericRange", class(p$range))
  p
}


# Predefined hyperparameter names, in case no name is given for a specific hp.
# > x, y, z, a, b, ..., w
HP_NAMES <- c(tail(letters, -23), head(letters, 23))

#' Combine Hyperparameter objetcs into a Hyperparameter Space
#'
#' If there are unnamed Hyperparameters given, we start naming them from x, y, z, a, ...
#' Unique names are ensured.
#'
#' @param ... List of named or unnamed Hyperparameters
#'
#' @examples
#' HyperparameterSpace(p_int(0, 9), p_dbl(lower = 0))
#' hp(category = p_fct(c("a", "b", "c")), alpha = p_dbl(0, 1))
#' @export
HyperparameterSpace <- function(...) {
  hps <- list(...)
  checkmate::assertList(hps)

  # Assure no missing and unique names
  nm <- names(hps)
  if (is.null(nm)) {
    nm <- HP_NAMES[seq(length(hps))]
  } else {
    n.missing <- sum(nm == "")
    nm[nm == ""] <- HP_NAMES[!HP_NAMES %in% nm][seq(n.missing)]
  }
  names(hps) <- make.names(nm, unique = TRUE)

  structure(hps, class = "HyperparameterSpace")
}

#' @rdname HyperparameterSpace
#' @export
hp <- HyperparameterSpace

#' @import data.table
#' @export
print.HyperparameterSpace <- function(x, ...) {
  hp.types <- sapply(x, function(x) x$type)
  hp.ranges <- sapply(x, function(x) format(x$range))

  dt <- data.table::data.table(
    name = names(x),
    type = hp.types,
    range = hp.ranges
  )
  print(dt)
  invisible(dt)
}

#' Check if list values lie in a given HyperparameterSpace
#'
#' This checks if a named lists values lie in a given HyperparameterSpace.
#'
#' @param hp List of named Hyperparameter values
#' @param hp.space HyperparameterSpace used as reference
#'
#' @return List with names of `hp` and logical values indicating if the value lies in the given HyperparameterSpace
#'
#' @examples
#' hp.space <- hp(x = p_int(0, 9), y = p_dbl(lower = 0))
#' checkHyperparameter(list(x = 5, y = 2.5), hp.space)
#' @import checkmate
#' @export
checkHyperparameter <- function(hp, hp.space) {
  checkmate::assertClass(hp.space, "HyperparameterSpace")
  checkmate::assertList(hp)
  checkmate::assertCharacter(names(hp), unique = TRUE, any.missing = FALSE, null.ok = FALSE)
  checkmate::assertSubset(names(hp), names(hp.space))

  setNames(
    lapply(
      names(hp),
      function(x) checkValidHP(hp.space[[x]], value = hp[[x]])
    ),
    names(hp)
  )
}

checkValidHP <- function(x, value, ...) UseMethod("checkValidHP", x)
checkValidHP.DoubleHyperparameter <- function(x, value, ...) {
  checkmate::qtest(value, sprintf("%s%s", "R", format(x$range)))
}
checkValidHP.IntegerHyperparameter <- function(x, value, ...) {
  if (is.infinite(value)) type <- "R" else type <- "X"
  checkmate::qtest(value, sprintf("%s%s", type, format(x$range)))
}
checkValidHP.FactorHyperparameter <- function(x, value, ...) {
  valid <- value %in% levels(x$range)
  if (!valid) cat(sprintf("Must be element of set %s, but is '%s'\n", format(x$range, max.display = TRUE), value))
  valid
}
checkValidHP.LogicalHyperparameter <- function(x, value, ...) {
  checkmate::qtest(value, "B")
}


hyperparameters <- function(x) {
  UseMethod("hyperparameters", x)
}
hyperparameters.default <- function(x) {
  if (is.null(attr(x, "hyperparameters"))) stop(sprintf("Object %s has no hyperparameter attribute", x))
  x$hyperparameters
}
