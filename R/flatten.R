#' Flatten a List or Other Object
#'
#' Reduces the depth of a list or other object. Most non-atomic objects (matrix,
#' data.frame, environments, etc.) are converted to a "list" in the first level
#' flattening. Atomic vectors, functions, and other special objects return themselves.
#'
#' @param x object of any class, but primarily designed for lists and other "deep" objects
#' @param level numeric integer describing the depth at which to flatten the object. If \code{level < 0}, the object will become as flat as possible.
#' @param ... objects passed to methods
#'
#' @details
#' \code{flatten} maps itself to each object with the aggregate \code{x}, combining
#' the results. Each time it is mapped, the level is reduced by 1. When \code{level == 0},
#' or an atomic vector or other special object is reached, \code{flatten} returns
#' the object without mapping itself.
#'
#' @return flatter object
#' @export
#'
#' @examples
#' x <- list(a = 1, b = 2:5, c = list(list(1,2,3), 4, 5), 6)
#' flatten(x)
#' ## returns: [1 2 3 4 5 1 2 3 4 5 6]
#'
#' flatten(x, level=1)
#' ## returns: [1 2 3 4 5 [1 2 3] 4 5 6]
#'
flatten <- function(x, level = -1, ...) UseMethod("flatten")
#' @method flatten default
#' @export
flatten.default <- function(x, level = -1, ...) return(x)
#' @method flatten environment
#' @export
flatten.environment <- function(x, level = -1, ...){
  if (level[1] == 0) return(x)
  flatten(as.list(x), level=level[1]-1)
}
#' @method flatten list
#' @export
flatten.list <- function(x, level = -1, ...){
  if (level[1] == 0) return(x)
  Reduce(c, lapply(x, flatten, level=level[1]-1, ...))
}
#' @method flatten matrix
#' @export
flatten.matrix <- function(x, level = -1, ...){
  if (level[1] == 0) return(x)
  flatten(cols(x), level=level[1]-1)
}
#' @method flatten data.frame
#' @export
flatten.data.frame <- function(x, level = -1, ...){
  if (level[1] == 0) return(x)
  flatten(as.list(x), level=level[1]-1)
}
