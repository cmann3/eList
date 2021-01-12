#' Higher Order Functions
#'
#' Common functions used in functional programming. These are similar to their
#' respective counterparts in Base R: \code{\link[base]{Map}}, \code{\link[base]{Reduce}},
#' \code{\link[base]{Filter}}, etc. However, they take any value that can be converted
#' to a function via \code{\link{lambda}} and the function comes after the argument
#' in the argument list for convenience while piping.
#'
#' @param x vector
#' @param f function to be applied to \code{x}
#' @param init object used to initialize \code{reduce}, if an object other then the first value is desired
#' @param right logical; should the reduction start from left (default) or right?
#' @param ... additional arguments supplied to \code{f}, or a list of functions for \code{compose} (applied in the order provided)
#' @param simplify logical; should the results be simplified to an array?
#' @param clust cluster to use for \code{\link[parallel:clusterApply]{parallel}} computations. See \code{\link{comprehension}} for more details. \code{FUN.VALUE} is ignored if a cluster is supplied.
#' @param USE.NAMES logical; should \code{x} be used as names for the result?
#' @param FUN.VALUE template vector for the return type
#'
#' @details
#' \code{map} is slightly different from \code{\link[base]{Map}} in Base R other than
#' the argument order. First, \code{\link{iter}} is applied to the vector \code{x}
#' so that users can define behavior for custom classes. Second, \code{\link{lambda}}
#' is applied to \code{f}. \code{map} only works for a single vector. Use \code{mapn}
#' to use multiple vectors as the function argument. The \code{map} functions are
#' wrappers around \code{\link[base]{sapply}} or \code{link[base]{vapply}} (if \code{FUN.VALUE} is provided).
#'
#' The other functions behave similar to what one would expect, with the exception
#' of \code{accumulate}. \code{accumulate} does not produce all intermediate results;
#' it only provides the final cumulative vector.
#'
#' \code{compose} takes multiple functions and produces a single "composed" function.
#' When the composed function is called, each function in the list is applied sequentially
#' to the arguments. The functions can be retrieved from the composed function's
#' attributes.
#'
#' @return determined by the return value of the function \code{f}.
#' @name funprog
#'
#' @examples
#' x <- list(1:3, 4:6, 7:9)
#' map(x, ~Reduce(`-`, .i))
#' map(x, sqrt)
#'
#' filter(x[[1]], ~.i < 3)
#'
#' reduce(x[[3]], `*`, init=1)
#'
#' f <- compose(sqrt, log, `*`(2))
#' f(10)
NULL

#' @describeIn funprog Apply function \code{f} using elements in vector \code{x} at each index.
#' @export
map <- function(x, f, ..., simplify=FALSE, USE.NAMES=FALSE, FUN.VALUE=NULL, clust=NULL){
  x <- iter(x)
  f <- lambda(f)
  if (!is.null(clust)) return(parSapply(clust, x, f, ..., simplify=simplify, USE.NAMES = USE.NAMES))
  if (is.null(FUN.VALUE)) return(sapply(x, f, ..., simplify=simplify, USE.NAMES = USE.NAMES))
  vapply(x, f, FUN.VALUE, ..., USE.NAMES = USE.NAMES)
}
#' @describeIn funprog Apply function \code{f} using the element in all elements in vectors \code{...} at each index as arguments.
#' @export
mapn <- function(f, ..., simplify=FALSE, USE.NAMES=FALSE, FUN.VALUE=NULL, clust=NULL){
  f <- lambda(f)
  l <- zip(...)
  if (!is.null(clust)) return(parSapply(clust, l, function(i) do.call(f, i), simplify=simplify, USE.NAMES = USE.NAMES))
  if (is.null(FUN.VALUE)) return(sapply(l, function(i) do.call(f, i), simplify=simplify, USE.NAMES = USE.NAMES))
  vapply(l, function(i) do.call(f, i), FUN.VALUE, USE.NAMES = USE.NAMES)
}

#' @describeIn funprog Find the position of elements in vector \code{x} that satisfy predicate function \code{f}.
#' @export
ffind <- function(x, f, ...) which(sapply(iter(x), lambda(f), ...))
#' @describeIn funprog Extract elements in vector \code{x} that satisfy predicate function \code{f}.
#' @export
filter <- function(x, f, ...) x[which(sapply(iter(x), lambda(f), ...))]
#' @describeIn funprog Combine elements in a vector \code{x} using binary function \code{f}.
#' @export
reduce <- function(x, f, init=NULL, right=FALSE){
  if (is.null(init)) return(Reduce(lambda(f), iter(x), right))
  Reduce(lambda(f), iter(x), init, right)
}
#' @describeIn funprog Combine elements in a vector \code{x} using binary function \code{f}, accumulating the results.
#' @export
accumulate <- function(x, f, init, right = FALSE){
  if (is.null(init)){y <- Reduce(lambda(f), iter(x), right, accumulate=TRUE)
  } else {y <- Reduce(lambda(f), iter(x), init, right, accumulate=TRUE)}
  y[[length(y)]]
}
#' @describeIn funprog Combine functions into a single function.
#' @export
compose <- function(...){
  dots <- eval(substitute(alist(...)))
  l <- lapply(dots, function(i) lambda(i))
  structure(
    function(x){
      funs <- attr(sys.function(), "funs")
      for (f in funs) x <- f(x)
      return(x)
    },
    funs = l,
    class = c("composition", "function")
  )
}
