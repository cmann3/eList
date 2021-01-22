#' Vectorized Comprehension in R
#'
#' Functions that provide Python-style list (and related) comprehension.
#' Comprehensions convert \code{\link[base:Control]{for}} loops into \code{\link[base:lapply]{lapply}} functions
#' before evaluation. Support for multiple variables, name assignment, nested loops,
#' custom iterators, if-else statements, and variety of return types included.
#'
#' @param loop a \code{for} loop with format: \code{for (var in seq) <name => <if (cond)> expr}. See "details" below.
#' @param map function, such as \code{lapply}, that is used for the comprehension
#' @param fun function to be called on result after comprehension
#' @param drop.names logical; should names be dropped after conversion? Defaults to \code{FALSE}.
#' @param clust cluster to use for \code{\link[parallel:clusterApply]{parallel}} computations
#' @param by.col should comprehension on matrix group by columns or rows? Defaults to \code{TRUE}.
#'
#' @details
#' The comprehension functions parse an R loop expression into \code{\link[base:lapply]{lapply}}
#' functions to allow for more readable code and easy creation and conversion
#' of vectors. The general syntax for a loop expression is as follows:
#'
#' \code{for (var in seq) <name=> <if (cond)> expr}
#'
#' where \code{<...>} denotes optional statements. The \code{seq} can be any R object:
#' a list, matrix, data.frame, environment, function, etc. The function \code{\link{iter}}
#' is called on the \code{seq}. So the behavior can be easily described for custom
#' classes or objects. See \code{\link{helpers}} for functions like \code{\link{zip}}
#' that can be used with \code{seq}.
#'
#' Multiple variables can be used in \code{var} by separating the names with a period ".".
#' For example, \code{i.j} is equivalent looping with variables \code{i} and \code{j}.
#' The downside is that periods cannot be used in the \code{var} name. When multiple variables
#' are used, the object received from the sequence at each iteration is split and its
#' elements assigned in order to each of the variables. If the \code{var} is \code{i.j} and
#' the object received in the iteration was \code{c(2,4,6)}, then \code{i=2}, \code{j=4},
#' and 6 would not be assigned. Since variables are split on periods, \code{i..j} could
#' be used to assign the first and third elements, or \code{.i.j} the second and third.
#' Any number of variables can be used. Note that the entire object is returned if
#' there are no periods in the name, so use \code{i..} if only the first object is needed.
#'
#' To provide names within a loop, preface the expression with the desired \code{name} for
#' that particular object followed by \code{=}. \code{name} can be any expression, just
#' make sure to surround any \code{if} chain for the name with parentheses, or the R
#' parser will not detect that the assignment operator is associated with the \code{expr}.
#' Behind the scenes, the expression on the left-hand side of \code{"="} is wrapped in
#' an \code{sapply} function and the results are assigned to the \code{\link[base:names]{names}}
#' of the right-hand side result.
#'
#' The \code{if} statement can contain any number of \code{if-else} statements and can
#' be nested. Similarly, \code{for} statements can be nested any number of times and
#' converted to \code{lapply} as long as the expression is a self-contained \code{for} loop.
#'
#' Though comprehensions are functions, both \code{List(for ...)} and \code{List[for ...]}
#' syntax are supported. See \code{\link{..}} for a convenience wrapper around \code{Vec}.
#'
#' The different comprehensions primarily describe the return value, with \code{List}
#' return a "list" and \code{Num} returning a numeric vector. If the object cannot be
#' converted, then an error will be produced. For \code{Env}, the objects must be
#' named. This means that either the name must be assigned within the loop or the
#' loop is performed across a named object and the name is preserved. Another
#' difference is that is some comprehensions - though related to atomic vectors -
#' convert \code{for} to \code{sapply} while others convert to \code{lapply}.
#'
#' The \code{Comp} function is used to create custom comprehensions. It should be
#' supplied with a \code{map} function such as \code{\link[base:lapply]{lapply}} that
#' accepts arguments: \code{X} for the argument over which the comprehension
#' iterates, \code{FUN} a function applied to each element, and \code{...} for
#' additional arguments passed to the \code{FUN}. \code{Comp} also accepts a
#' post-evaluation function, \code{fun}, that is applied to the result. This
#' could be used to ensure that the result complies to some class or other
#' restriction.
#'
#' Users can also specify a cluster to use. If specified, then a parallel version
#' of \code{lapply} or \code{sapply} is used based on \code{parLapply} and \code{parSapply}
#' from the \code{\link[parallel:clusterApply]{parallel}} package. This can greatly
#' reduce the calculation time for different operations, but has additional overhead
#' that makes the cost greater than the benefit for relatively small vectors. See
#' \code{\link{auto_cluster}} for auto-creation.
#'
#' @return Determined by the function. \code{List} returns an object of class 'list',
#' \code{Num} returns a numeric vector, etc. See the descriptions of each function for
#' their return type.
#'
#' @name comprehension
#' @examples
#' people <- list(
#'   John = list(age = 30, weight = 180, mood = "happy", gender = "male"),
#'   April = list(age = 26, weight = 110, mood = "sad", gender = "female"),
#'   Jill = list(age = 42, weight = 125, mood = "ok", gender = "female")
#' )
#'
#' weight_kg <- Num(for (i in people) i$weight/2.2)
#' gender <- Chr(for (i in people) i$gender)
#' gender_tab <- List(for (i in c("male", "female")) i = length(which(gender == i)))
#'
#' Chr(for (..i.j in people) paste0(i, " & ", j))
#'
#' Chr(for (i.j in items(people)) paste0(i, " is ", j$age, " years old."))
#'
#' e <- Env(for (i.j in items(people)) i = j$age)
#' e$John
#'
#' Num(for (i in 1:10) for (j in 2:6) if (i == j) i^2)
#'
NULL

#' @describeIn comprehension Create generalized comprehension function
#' @export
Comp <- function(map = lapply, fun=NULL){
  structure(
    function(loop, ...){
      mapfun <- attr(sys.function(), "mapfun")
      fun  <- attr(sys.function(), "fun")
      ret <- eval(parse(text=.parse_for(substitute(loop), mapfun=mapfun, ...)))
      if (is.null(fun)) return(ret)
      fun(ret)
    }, mapfun=map, fun=fun, class=c("Comprehension", "function")
  )
}

#' @describeIn comprehension Generate a 'list' from a \code{for} loop
#' @importFrom parallel parLapply
#' @export
List <- structure(function(loop, clust=NULL, fun=NULL){
  if (is.null(clust)){mapfun <- lapply
  } else { mapfun <- function(X, FUN) parallel::parLapply(clust, X, FUN)}
  CURR_ENV <- environment()
  pt <- parse(text=.parse_for(substitute(loop), mapfun=mapfun))
  parent.env(CURR_ENV) <- sys.frame()
  ret <- eval(pt, CURR_ENV)
  if (is.null(fun)) return(ret)
  fun(ret)
}, mapfun = lapply, class = c("Comprehension", "function"))
#' @describeIn comprehension Generate an 'environment' from a \code{for} loop
#' @importFrom parallel parLapply
#' @export
Env <- structure(function(loop, clust=NULL){
  if (is.null(clust)){mapfun <- lapply
  } else {mapfun <- function(X, FUN) parallel::parLapply(clust, X, FUN)}
  CURR_ENV <- environment()
  pt <- parse(text=.parse_for(substitute(loop), mapfun=mapfun))
  parent.env(CURR_ENV) <- sys.frame()
  as.environment(eval(pt, CURR_ENV))
}, mapfun = lapply, fun = as.environment, class = c("Comprehension", "function"))
#' @describeIn comprehension Generate a flat, atomic 'vector' from a \code{for} loop
#' @importFrom parallel parSapply
#' @export
Vec <- structure(function(loop, clust=NULL, drop.names = FALSE){
  mapfun <- sapply
  if (!is.null(clust)) mapfun <- function(X, FUN, ...) parallel::parSapply(clust, X, FUN, ...)
  #if (is.null(clust)){ mapfun <- sapply
  #} else {mapfun <- function(X, FUN, ...) parSapply(clust, X, FUN, ...)}
  CURR_ENV <- environment()
  pt <- parse(text=.parse_for(substitute(loop), mapfun=mapfun))
  parent.env(CURR_ENV) <- sys.frame()
  ret <- eval(pt, CURR_ENV)
  if (drop.names) return(as.vector(flatten(ret)))
  .save_names(ret, function(i){as.vector(flatten(i))})
}, mapfun = sapply, fun = function(i){as.vector(flatten(i))}, class = c("Comprehension", "function"))
#' @describeIn comprehension Generate a 'numeric' vector from a \code{for} loop
#' @importFrom parallel parSapply
#' @export
Num <- structure(function(loop, clust=NULL, drop.names = FALSE){
  if (is.null(clust)){mapfun <- sapply
  } else {mapfun <- function(X, FUN, ...) parallel::parSapply(clust, X, FUN, ...)}
  CURR_ENV <- environment()
  pt <- parse(text=.parse_for(substitute(loop), mapfun=mapfun))
  parent.env(CURR_ENV) <- sys.frame()
  ret <- eval(pt, CURR_ENV)
  if (drop.names) return(as.numeric(flatten(ret)))
  .save_names(ret, function(i){as.numeric(flatten(i))})
}, mapfun = sapply, fun = function(i){as.numeric(flatten(i))}, class = c("Comprehension", "function"))
#' @describeIn comprehension Generate a 'character' vector from a \code{for} loop
#' @importFrom parallel parSapply
#' @export
Chr <- structure(function(loop, clust=NULL, drop.names = FALSE){
  if (is.null(clust)){mapfun <- sapply
  } else {mapfun <- function(X, FUN, ...) parallel::parSapply(clust, X, FUN, ...)}
  CURR_ENV <- environment()
  pt <- parse(text=.parse_for(substitute(loop), mapfun=mapfun))
  parent.env(CURR_ENV) <- sys.frame()
  ret <- eval(pt, CURR_ENV)
  if (drop.names) return(as.character(flatten(ret)))
  .save_names(ret, function(i){as.character(flatten(i))})
}, mapfun = sapply, fun = function(i){as.character(flatten(i))}, class = c("Comprehension", "function"))
#' @describeIn comprehension Generate a 'logical' vector from a \code{for} loop
#' @importFrom parallel parSapply
#' @export
Logical <- structure(function(loop, clust=NULL, drop.names = FALSE){
  if (is.null(clust)){mapfun <- sapply
  } else {mapfun <- function(X, FUN, ...) parallel::parSapply(clust, X, FUN, ...)}
  CURR_ENV <- environment()
  pt <- parse(text=.parse_for(substitute(loop), mapfun=mapfun))
  parent.env(CURR_ENV) <- sys.frame()
  ret <- eval(pt, CURR_ENV)
  if (drop.names) return(as.logical(flatten(ret)))
  .save_names(ret, function(i){as.logical(flatten(i))})
}, mapfun = sapply, fun = function(i){as.logical(flatten(i))}, class = c("Comprehension", "function"))
#' @describeIn comprehension Generate a 'matrix' from a \code{for} loop
#' @importFrom parallel parSapply
#' @export
Mat <-  structure(function(loop, clust=NULL, by.col=TRUE){
  if (is.null(clust)){
    if (by.col){mapfun <- function(x, f, ...) sapply(cols(x), f, ...)
    } else {mapfun <- function(x, f, ...) sapply(rows(x), f, ...)}
    mapfun <- sapply
  } else {
    if (by.col){mapfun <- function(x, f, ...) parallel::parSapply(clust, cols(x), f, ...)
    } else {mapfun <- function(x, f, ...) parallel::parSapply(clust, rows(x), f, ...)}
  }
  CURR_ENV <- environment()
  pt <- parse(text=.parse_for(substitute(loop), mapfun=mapfun))
  parent.env(CURR_ENV) <- sys.frame()
  as.matrix(eval(pt, CURR_ENV))
}, mapfun = function(x, f, ...) sapply(cols(x), f, ...), fun = as.matrix, class = c("Comprehension", "function"))
#' @describeIn comprehension Generate a 'data.frame' from a \code{for} loop
#' @importFrom parallel parLapply
#' @export
DF <-  structure(function(loop, clust=NULL){
  if (is.null(clust)){mapfun <- lapply
  } else {mapfun <- function(X, FUN) parallel::parLapply(clust, X, FUN)}
  CURR_ENV <- environment()
  pt <- parse(text=.parse_for(substitute(loop), mapfun=mapfun))
  parent.env(CURR_ENV) <- sys.frame()
  as.data.frame(eval(pt, CURR_ENV))
}, mapfun = lapply, fun = as.data.frame, class = c("Comprehension", "function"))


#' Create Vector
#'
#' The \code{..} function allows for the quick creation of vector using either
#' \code{..(...)} or \code{..[...]}. It accepts vector \code{\link{comprehension}}
#' arguments using \code{for ...}. It can also be used as a more general form of
#' \code{\link[base:c]{c}}.
#'
#' @param ... values to be combined within a vector. Arguments beginning with \code{for} are interpreted as comprehensions.
#' @param clust cluster to use for \code{\link[parallel:clusterApply]{parallel}} computations
#' @param type \code{\link{comprehension}} function used when \code{for} arguments are present. Defaults to \code{Vec}.
#' @param simplify logical; should the result be simplified to an array if possible?
#'
#' @return vector
#' @importFrom parallel parLapply parSapply
#' @export
#'
#' @examples
#' ..[for (i in 1:10) 2*(1:i)]
#'
.. <- structure(
  function(..., clust=NULL, type=Vec, simplify=TRUE){
    dots <- eval(substitute(alist(...)))
    fun <- attr(type, "fun")
    if (is.null(clust)){mapfun <- attr(type, "mapfun")
    } else {
      mfun <- attr(type, "mapfun")
      if (identical(mfun, sapply)){
        mapfun <- function(X, FUN) parSapply(clust, X, FUN)
      } else {function(X, FUN) parLapply(clust, X, FUN)}
    }

    l <- lapply(dots, function(i){
      e <- eval(parse(text=.parse_for(i, mapfun=mapfun)))
      if (is.null(fun)) return(e)
      fun(e)
    })
    if (length(dots) == 1){l <- l[[1]]
    } else if (simplify){l <- Reduce(c, l)}
    l
  },
  class = c("VecGen", "function")
)


#' Vectorized Comprehension and Summary
#'
#' Functions that summarize the results of a Python-style comprehension. These functions
#' extend those in \code{\link{comprehension}} by applying a post-evaluation function to
#' the results of the loop.
#'
#' @param ... vectors of any type or a \code{for} loop with format: \code{for (var in seq) <name => <if (cond)> expr}. See \code{\link{comprehension}}.
#' @param na.rm logical; should missing values be removed? Defaults to \code{FALSE}
#' @param collapse character describing how the results from \code{Paste} should be collapsed. See \code{\link[base:paste]{paste}}.
#' @param trim fraction between 0 and 0.5 describing percent of observations to be trimmed from each side for the mean
#' @param clust cluster to use for \code{\link[parallel:clusterApply]{parallel}} computations
#'
#' @return Single numeric or character value, or a list of results for \code{Stats}
#'
#' @name comprehendSummary
#'
#' @examples
#' ## Calculate the sum of all even numbers to 100
#' Sum(for (i in seq(2, 100, 2)) i)
#'
#' ## Find the mean
#' Mean(for (i in 1:10) log(i))
#'
#' ## Combine character values
#' greet <- c("Hello", "World", "Nice", "To", "Meet", "You")
#' val <- Paste(for (i.j in enum(greet)) paste0(i, ": ", j), collapse="\n")
#' cat(val)
#'
NULL

#' @describeIn comprehendSummary Are all results \code{TRUE}?
#' @importFrom parallel parSapply
#' @export
All <- structure(function(..., clust=NULL, na.rm=FALSE){
  dots <- eval(substitute(alist(...)))
  if (is.null(clust)){mapfun <- sapply
  } else {mapfun <- function(X, FUN, ...) parSapply(clust, X, FUN, ...)}
  all(sapply(dots, function(i) all(eval(parse(text=.parse_for(i, mapfun=mapfun))), na.rm=na.rm)), na.rm=na.rm)
}, class = c("Comprehension", "function"))
#' @describeIn comprehendSummary Are any results \code{TRUE}?
#' @importFrom parallel parSapply
#' @export
Any <- structure(function(..., clust=NULL, na.rm=FALSE){
  dots <- eval(substitute(alist(...)))
  if (is.null(clust)){mapfun <- sapply
  } else {mapfun <- function(X, FUN, ...) parSapply(clust, X, FUN, ...)}
  any(sapply(dots, function(i) any(eval(parse(text=.parse_for(i, mapfun=mapfun))), na.rm=na.rm)), na.rm=na.rm)
}, class = c("Comprehension", "function"))
#' @describeIn comprehendSummary Are all results \code{FALSE}?
#' @importFrom parallel parSapply
#' @export
None <- structure(function(..., clust=NULL, na.rm=FALSE){
  dots <- eval(substitute(alist(...)))
  if (is.null(clust)){mapfun <- sapply
  } else {mapfun <- function(X, FUN, ...) parSapply(clust, X, FUN, ...)}
  !any(sapply(dots, function(i) any(eval(parse(text=.parse_for(i, mapfun=mapfun))), na.rm=na.rm)), na.rm=na.rm)
}, class = c("Comprehension", "function"))
#' @describeIn comprehendSummary Calculate the \code{\link[base]{sum}} of results
#' @importFrom parallel parSapply
#' @export
Sum   <- structure(function(..., clust=NULL, na.rm=FALSE){
  dots <- eval(substitute(alist(...)))
  if (is.null(clust)){mapfun <- sapply
  } else {mapfun <- function(X, FUN, ...) parSapply(clust, X, FUN, ...)}
  sum(sapply(dots, function(i) sum(eval(parse(text=.parse_for(i, mapfun=mapfun))), na.rm=na.rm)), na.rm=na.rm)
}, class = c("Comprehension", "function"))
#' @describeIn comprehendSummary Calculate the \code{\link[base]{prod}} of results
#' @importFrom parallel parSapply
#' @export
Prod  <- structure(function(..., clust=NULL, na.rm=FALSE){
  dots <- eval(substitute(alist(...)))
  if (is.null(clust)){mapfun <- sapply
  } else {mapfun <- function(X, FUN, ...) parSapply(clust, X, FUN, ...)}
  prod(sapply(dots, function(i) prod(eval(parse(text=.parse_for(i, mapfun=mapfun))), na.rm=na.rm)), na.rm=na.rm)
}, class = c("Comprehension", "function"))
#' @describeIn comprehendSummary Find the minimum in the result
#' @importFrom parallel parSapply
#' @export
Min   <- structure(function(..., clust=NULL, na.rm=FALSE){
  dots <- eval(substitute(alist(...)))
  if (is.null(clust)){mapfun <- sapply
  } else {mapfun <- function(X, FUN, ...) parSapply(clust, X, FUN, ...)}
  min(sapply(dots, function(i) min(eval(parse(text=.parse_for(i, mapfun=mapfun))), na.rm=na.rm)), na.rm=na.rm)
}, class = c("Comprehension", "function"))
#' @describeIn comprehendSummary Find the maximum in the result
#' @importFrom parallel parSapply
#' @export
Max   <- structure(function(..., clust=NULL, na.rm=FALSE){
  dots <- eval(substitute(alist(...)))
  if (is.null(clust)){mapfun <- sapply
  } else {mapfun <- function(X, FUN, ...) parSapply(clust, X, FUN, ...)}
  max(sapply(dots, function(i) max(eval(parse(text=.parse_for(i, mapfun=mapfun))), na.rm=na.rm)), na.rm=na.rm)
}, class = c("Comprehension", "function"))
#' @describeIn comprehendSummary Calculate the arithmetic mean of the result
#' @importFrom parallel parSapply
#' @export
Mean  <- structure(function(..., clust=NULL, na.rm=FALSE, trim=0){
  dots <- eval(substitute(alist(...)))
  if (is.null(clust)){mapfun <- sapply
  } else {mapfun <- function(X, FUN, ...) parSapply(clust, X, FUN, ...)}
  mean(sapply(dots, function(i) mean(eval(parse(text=.parse_for(i, mapfun=mapfun))), na.rm=na.rm, trim=trim)), na.rm=na.rm, trim=trim)
}, class = c("Comprehension", "function"))
#' @describeIn comprehendSummary Find the 7 number summary (5 number + mean & sd) of the result
#' @importFrom parallel parSapply
#' @importFrom stats quantile sd
#' @export
Stats <- structure(function(..., clust=NULL, na.rm=FALSE, trim=0){
  dots <- eval(substitute(alist(...)))
  if (is.null(clust)){mapfun <- sapply
  } else {mapfun <- function(X, FUN, ...) parSapply(clust, X, FUN, ...)}
  x <- Reduce(c, sapply(dots, function(i) eval(parse(text=.parse_for(i, mapfun=mapfun)))))
  q <- quantile(x, probs=seq(0,1,0.25), na.rm=na.rm, names=FALSE)
  list(
    min  = q[1],
    q1   = q[2],
    med  = q[3],
    q3   = q[4],
    max  = q[5],
    mean = mean(x, na.rm=na.rm, trim=trim),
    sd   = sd(x, na.rm=na.rm)
  )
}, class = c("Comprehension", "function"))
#' @describeIn comprehendSummary Collapse the result into a single character
#' @importFrom parallel parSapply
#' @export
Paste <- structure(function(..., clust=NULL, collapse=""){
  dots <- eval(substitute(alist(...)))
  if (is.null(clust)){mapfun <- sapply
  } else {mapfun <- function(X, FUN, ...) parSapply(clust, X, FUN, ...)}
  paste(sapply(dots, function(i) paste(eval(parse(text=.parse_for(i, mapfun=mapfun))), collapse=collapse)), collapse=collapse)
}, class = c("Comprehension", "function"))
