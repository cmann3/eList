#' Higher Order Helpers for Vector Comprehension
#'
#' These functions use a vector and a function to create an iterable object that
#' can be used for vector comprehension.
#'
#' @param x vector
#' @param f function to be applied to \code{x}
#' @param ... additional arguments passed to lower functions. See \code{\link{funprog}}
#' @param axis which axis to perform different operations? \code{axis=0}, the default, performs operations on each element in the list (columns), while \code{axis=1} performs operations on each object within the elements of a list (rows).
#' @param fill object with which to fill the vector when operating on elements with varying lengths or shifts.
#' @param longest logical; should the longest item be used to determine the new length or shortest? Defaults to \code{TRUE}.
#'
#' @details
#' The \code{star} functions are similar to their \code{\link{funprog}} counterparts,
#' except that they are applied one level deeper in the list.
#'
#' @return list or other vector
#'
#' @name helpersFun
#'
#' @examples
#' x <- list(1:3, 4:6, 7:9)
#'
#' ## filter away values less than 6
#' starfilter(x, ~.i > 5)
#' starfilter(x, ~.i > 5, axis=1) # Transposed
#'
#' starred(x, `/`, init=1) # sequentially divide each item, starting at 1
#'
#' partition(x, ~.i > 5)
NULL

#' @describeIn helpersFun Use \code{map} \code{f} on each element of \code{x}.
#' @export
starmap <- function(x, f, axis = 0, ..., longest=TRUE, fill=NULL){
  f <- lambda(f)
  x <- iter(x)
  if (axis == 1) x <- transpose(x, longest= longest, fill=fill)
  lapply(x, function(i) map(i, f, ...))
}
#' @describeIn helpersFun Use \code{reduce} \code{f} on each element of \code{x}.
#' @export
starred <- function(x, f, axis = 0, ..., longest=TRUE, fill=NULL){
  f <- lambda(f)
  x <- iter(x)
  if (axis == 1) x <- transpose(x, longest= longest, fill=fill)
  Reduce(c, lapply(x, function(i) reduce(i, f, ...)))
}
#' @describeIn helpersFun Use \code{filter} \code{f} on each element of \code{x}.
#' @export
starfilter <- function(x, f, axis = 0, ..., longest=TRUE, fill=NULL){
  f <- lambda(f)
  x <- iter(x)
  if (axis == 1) x <- transpose(x, longest= longest, fill=fill)
  lapply(x, function(i) filter(i, f, ...))
}
#' @describeIn helpersFun Map predicate function \code{f} to each object in \code{x} and split based on which items evaluate to \code{TRUE} (index 1) vs. \code{FALSE} (index 2).
#' @export
partition <- function(x, f, ...){
  f <- lambda(f)
  lapply(iter(x), function(i){
    w <- which(as.logical(map(i, f, ...)))
    if (length(w) == 0) return(list(i[w], i))
    list(i[w], i[-w])
  })
}
#' @describeIn helpersFun Drop objects from \code{x} until predicate function \code{f} evaluates to \code{FALSE}.
#' @export
dropwhile <- function(x, f, ...){
  f <- lambda(f)
  x <- iter(x)
  i <- 1
  n <- length(x)
  while (i <= n){
    if (any(f(x[[i]],...))) break
    i <- i + 1
  }
  if (i <= n) return(x[i:n])
  list()
}
#' @describeIn helpersFun Keep objects from \code{x} until predicate function \code{f} evaluates to \code{FALSE}.
#' @export
takewhile <- function(x, f, ...){
  f <- lambda(f)
  x <- iter(x)
  i <- 1
  n <- length(x)
  while (i <= n){
    if (!any(f(x[[i]], ...))) break
    i <- i + 1
  }
  if (i > 0) return(x[1:i])
  list()
}
