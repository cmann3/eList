#' Remove 'NULL' Entries from List
#'
#' Function removes all items that are \code{NULL} or empty from a list
#' or other object.
#'
#' @param x object to be checked
#'
#' @return \code{x} without \code{NULL} entries
#'
#' @export
#' @examples
#' l <- list(a=2, b=NULL, c = 3)
#' length(l) == 3
#'
#' k <- null.omit(l)
#' length(k) == 2
#'
null.omit <- function(x) UseMethod("null.omit")
#' @method null.omit default
#' @export
null.omit.default <- function(x) x
#' @method null.omit list
#' @export
null.omit.list <- function(x) x[!(sapply(x, function(i) is.null(i)))]

