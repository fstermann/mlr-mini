Hyperparameter <- function(type, range) {
  # checkmate::assert_character(name)
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

format.Range <- function(x, ...) paste(x, collapse = ", ")

format.NumericRange <- function(x, ...) sprintf("[%s, %s]", x[1], x[2])

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

print.Range <- function(x, ...) {
  cat(format(x))
  invisible(x)
}

#' @title Double Hyperparameter
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

#' @title Integer Hyperparameter
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

#' @title Factor Hyperparameter
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

#' @title Logical/Boolean Hyperparameter
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
hp <- HyperparameterSpace


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

checkHyperparameter <- function(hp, hp.space) {
  checkmate::assertClass(hp.space, "HyperparameterSpace")
  checkmate::assertList(hp)
  checkmate::assertSubset(names(hp), names(hp.space))

  setNames(
    lapply(
      names(hp),
      function(x) assertValidHP(hp.space[[x]], value = hp[[x]])
    ),
    names(hp)
  )
}

assertValidHP <- function(x, value, ...) UseMethod("assertValidHP", x)
assertValidHP.DoubleHyperparameter <- function(x, value, ...) {
  checkmate::qtest(value, sprintf("%s%s", "R", format(x$range)))
}
assertValidHP.IntegerHyperparameter <- function(x, value, ...) {
  if (is.infinite(value)) type <- "R" else type <- "X"
  checkmate::qtest(value, sprintf("%s%s", type, format(x$range)))
}
assertValidHP.FactorHyperparameter <- function(x, value, ...) {
  valid <- value %in% levels(x$range)
  if (!valid) cat(sprintf("Must be element of set %s, but is '%s'\n", format(x$range, max.display = TRUE), value))
  valid
}
assertValidHP.LogicalHyperparameter <- function(x, value, ...) {
  checkmate::assertLogical(value)
  value %in% x$range # Probably not needed, since it would be a constant with length(range()) == 1
}



hyperparameters <- function(x) {
  UseMethod("hyperparameters", x)
}
