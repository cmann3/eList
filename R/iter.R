#' Create an Iterable Object
#'
#' Vector \code{\link{comprehension}} iterates over an object, but the default
#' behavior may not be desirable for custom classes. \code{iter} allows the
#' user to specify how the object behaves within a comprehension, or other loop
#' in the \code{eList} package. Unless a method is specified for an object,
#' iter will attempt to convert it to a list except for atomic vectors.
#'
#' @param x object to be looped across
#'
#' @return a vector
#' @export
#' @examples
#' e <- new.env()
#' e$x <- 10
#' e$y <- letters[1:10]
#' iter(e)
#'
iter <- function(x) UseMethod("iter")
#' @export
iter.default <- function(x) as.list(x)
#' @export
iter.list <- function(x) x
#' @export
iter.character <- function(x) x
#' @export
iter.numeric <- function(x) x
#' @export
iter.complex <- function(x) x
#' @export
iter.logical <- function(x) x
