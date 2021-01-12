#' Helpers for Vector Comprehension
#'
#' These functions help to create sequences for use in vector \link{comprehension}.
#'
#' @param x list, environment, or other vector
#' @param n size of window for \code{roll} and \code{separate}, or position of item in which to split each element in \code{splitn}
#' @param k number of elements to shift right. Negative values of \code{k} shift to the left
#' @param g vector of objects used to define groups
#' @param axis which axis to perform different operations? \code{axis=0}, the default, performs operations on each element in the list (columns), while \code{axis=1} performs operations on each object within the elements of a list (rows).
#' @param fill object with which to fill the vector when operating on elements with varying lengths or shifts.
#' @param longest logical; should the longest item be used to determine the new length or shortest? Defaults to \code{TRUE}.
#' @param start,end,by integers of length 1 describing the sequence for slicing the vector. If missing, they will default to the start or end of the vector.
#' @param head logical; should \code{fill} be at the head of the vector or the tail?
#' @param ... vectors to combine
#'
#' @details
#' These functions transform vectors or other objects into lists, by adding elements,
#' grouping objects, extracting certain elements, and so forth. These can be used
#' in conjunction with vector \code{\link{comprehension}} to develop quick and
#' readable code.
#'
#' An example of how each of these can be used is seen here. Let \code{x} and \code{y}
#' be given as follows.
#'
#' \code{x = list(a = 2, b = 4, c = 8)}
#' \code{y = list(1:2, 2:3, 3:4)}
#'
#' Then the various helper functions will have the following effect.
#'
#' \itemize{
#'  \item \code{chain(y) => [1, 2, 2, 3, 3, 4]}
#'  \item \code{chars("hello") => ['h', 'e', 'l', 'l', 'o']}
#'  \item \code{enum(x) => [[1, 2], [2, 4], [3, 8]]}
#'  \item \code{first(y) => [1, 2, 3]}
#'  \item \code{groups(x, c("z", "w", "z")) => [["z", [2, 8]], ["w", [4]]]}
#'  \item \code{items(x) => [["a", 2], ["b", 4], ["c", 8]]}
#'  \item \code{lagg(x, 2) => [[2, 4, 8], [NA, 2, 4], [NA, NA, 2]]}
#'  \item \code{lrep(x, 3) => [[2, 4, 8], [2, 4, 8], [2, 4, 8]]}
#'  \item \code{rest(y) => [[2], [3], [4]]}
#'  \item \code{roll(x, 2) => [[2, 4] [4, 8]]}
#'  \item \code{separate(x, 2) => [[2, 4], [8, NA]]}
#'  \item \code{slice(x,1,,2) => [2, 8]}
#'  \item \code{splitn(y) => [[[1], [2]], [[2], [3]], [[3], [4]]]}
#'  \item \code{transpose(y) => [[1, 2, 3], [2, 3, 4]]}
#'  \item \code{unroll(y) => [1, 2, 3, 4]}
#'  \item \code{vals(x) => [2, 4, 8]}
#'  \item \code{zip(x, 1:3) => [[2, 1], [4, 2], [8, 3]]}
#' }
#'
#' @return list or other vector
#' @name helpers
#'
#' @examples
#' x <- 1:10
#' y <- 32:35
#'
#' n <- Num(for (i.j in zip(x,y)) i+j)
#' # Note that the result is different from x+y since the shortest does not repeat
#' mean(n[1:4])
#'
#' e <- new.env()
#' e$a <- 1:5
#' e$b <- 6:10
#'
#' e2 <- Env(for (key.val in items(e)) key = sqrt(val))
#' e2$a
#'
#' # row product
#' mat <- matrix(1:9, nrow=3)
#' Num(for (i in rows(mat)) prod(i))
NULL

#' @describeIn helpers Create a list containing the name of each element of \code{x} and its value.
#' @export
items <- function(x){
  nme <- names(x)
  if (is.null(nme)){
    nme <- rep("", length(x))
    warning("Object is not named. First element in 'items' will be \"\".")
  }
  if (is.environment(x)) return(lapply(nme, function(i) list(i, x[[i]])))
  lapply(seq_along(x), function(i) list(nme[i], x[[i]]))
}

#' @describeIn helpers Extract the values of x without their names.
#' @export
vals <- function(x){
  if (is.environment(x)) x <- as.list(x)
  names(x) <- NULL
  x
}

#' @describeIn helpers Create a list containing the index of each element of \code{x} and its value.
#' @export
enum <- function(x) lapply(seq_along(x), function(i) list(i, x[[i]]))

#' @describeIn helpers Create a list containing the rows of a data.frame or matrix
#' @export
rows <- function(x, ...) UseMethod("rows")
#' @export
rows.default <- function(x, ...) x
#' @export
rows.matrix <- function(x, ...) lapply(1:nrow(x), function(i) x[i,])
#' @export
rows.data.frame <- function(x, ...) lapply(1:nrow(x), function(i) x[i,])

#' @describeIn helpers Create a list containing the columns of a data.frame or matrix
#' @export
cols <- function(x, ...) UseMethod("cols")
#' @export
cols.default <- function(x, ...) x
#' @export
cols.matrix <- function(x, ...) lapply(1:ncol(x), function(i) x[,i])
#' @export
cols.data.frame <- function(x, ...) lapply(1:ncol(x), function(i) x[,i])


#' @describeIn helpers Merge two or more vectors into a list with each index containing values from each vector at that index.
#' @export
zip <- function(..., fill=NA, longest = TRUE){
  dots <- lapply(list(...), function(i) iter(i))
  n <- sapply(dots, length)
  N <- length(n)
  if (N == 0) return(list())
  if (longest){
    maxn <- max(n)
    l <- lapply(1:maxn, function(i){
      lapply(seq_along(dots), function(j){
        if (n[j] < i) return(fill)
        return(dots[[j]][[i]])
      })
    })
  } else {
    minn <- min(n)
    if (minn == 0) return(list())
    l <- lapply(1:minn, function(i){
      sapply(dots, function(item){return(item[[i]])})
    })
  }
  l
}

#' @describeIn helpers Repeat \code{x}, \code{n} times, with each repetition being an item in a list.
#' @export
lrep <- function(x, n=2, axis=0){
  if (axis == 0) return(lapply(1:n, function(i) x))
  lapply(x, function(i) rep(i, n))
}


#' @describeIn helpers Transpose a list or other object into a list. Opposite of \code{zip}.
#' @export
transpose <- function(x, fill=NA, longest = TRUE) UseMethod("transpose")
#' @export
transpose.default <- function(x, fill=NA, longest = TRUE) list(x)
#' @export
transpose.matrix <- function(x, fill=NA, longest = TRUE) rows(x)
#' @export
transpose.list <- function(x, fill=NA, longest = TRUE){
  nx <- length(x)
  if (nx == 0) return(x)
  ns <- sapply(x, length)
  if (longest){
    nmax <- max(ns)
    return(lapply(1:nmax, function(i){
      Reduce(c, lapply(1:nx, function(n){
        if (ns[n] < nmax) return(fill)
        x[[n]][[i]]
      }))
    }))
  }
  nmin <- min(ns)
  if (nmin == 0){
    warning("List contained an object of length 0. Transpose returns an empty list. Use 'longest=TRUE' to ensure that the result is populated.")
    return(list())
  }
  lapply(1:nmin, function(i) Reduce(c, lapply(1:nx, function(n) x[[n]][[i]])))
}



#' @describeIn helpers Subset an object by a sequence: \code{start}, \code{end}, \code{by}. If \code{start} is missing, it is assumed to be 1. If \code{end} is missing, it is assumed to be the length of the object.
#' @export
slice <- function(x, start, end, by=1L){
  if (missing(start)) start <- 1L
  if (missing(end)) end <- length(x)
  x[seq(start, end, by)]
}

#' @describeIn helpers Create a list of objects containing \code{n} items from \code{x}, with \code{n-1} elements overlapping in a chain. Opposite of \code{unroll}.
#' @export
roll <- function(x, n=2, fill=NULL, head=TRUE, ...){
  nx <- length(x)
  if (nx < n){
    warning("Object is smaller than window size. Returning object.")
    return(x)
  }
  l <- lapply(1:(nx-n+1), function(i) x[i:(i+n-1)])
  if (!is.null(fill)){
    if (head) return(c(rep(list(fill), n-1), l))
    return(c(l, rep(list(fill), n-1)))
  }
  l
}

#' @describeIn helpers Flatten a list by combining the unique elements between each group of two elements. Opposite of \code{roll}.
#' @export
unroll <- function(x){
  n <- length(x)
  if (n < 2) return(x)
  sq <- seq(2, n, 2)
  l <- Reduce(c, lapply(seq(2, n, 2), function(i){
    unique(c(x[[i-1]], x[[i]]))
  }))
  if (n %% 2 != 0){
    l <- c(l, x[[n]][which(!(x[[n]] %in% x[[n-1]]))])
  }
  l
}

#' @describeIn helpers Create a list containing an object and each the first \code{k} lags of an object.
#' @export
lagg <- function(x, k=1, fill=NA, axis = 0){
  n <- length(x)
  if (n <= k) stop("lagg size must be less than the length of the object.")
  l <- lapply(0:k, function(i){
    if (i == 0) return(x)
    if (i > 0) return(c(rep(fill, i), x[1:(n-i)]))
    return(c(x[(1-i):n], rep(fill, abs(i))))
  })
  if (axis == 0) return(l)
  transpose(l)
}

#' @describeIn helpers Create a list where each element is a list with the first element equal to a unique value in \code{g} and the other element is a list containing all values of \code{x} at the same indices as the value of \code{g}.
#' @export
groups <- function(x, g){
  keys <- unique(g)
  lapply(keys, function(i)  list(i, x[which(g == i)]))
}


#' @describeIn helpers Convert a character string into a vector of single character values.
#' @export
chars <- function(x) unlist(strsplit(x[1], ""))

#' @describeIn helpers Combine each object in a list. Opposite of \code{separate}.
#' @export
chain <- function(x) Reduce(c, x)

#' @describeIn helpers Separate vector into a list of objects with length \code{n}. Opposite of \code{chain}.
#' @export
separate <- function(x, n = 2, fill = NA){
  nx <- length(x)
  ntotal <- ceiling(nx/n)
  lapply(1:ntotal, function(i){
    start <- (i-1)*n + 1
    end   <- start + n - 1
    if (i == ntotal) return(c(x[start:nx], rep(fill, end-nx)))
    x[start:end]
  })
}

#' @describeIn helpers Take the first element of each item in a list.
#' @export
first <- function(x) lapply(x, function(i) i[[1]])
#' @describeIn helpers Remove the first element of each item in a list.
#' @export
rest <- function(x) lapply(x, function(i) i[-1])
#' @describeIn helpers Split each element in a list into two parts: one with the first \code{n} elements and the second with the rest.
#' @export
splitn <- function(x,n=1) lapply(x, function(i) list(i[1:n], i[-(1:n)]))

