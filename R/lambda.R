#' Create Functions from Formulas/Objects
#'
#' \code{lambda} allows the quick creation of anonymous functions from a variety
#' of different object types, such as formulas or from other calls.
#'
#' @param x object to be converted to a function
#' @param ... arguments passed to methods
#'
#' @details
#' The behavior of \code{lambda} depends on the object that is passed to it.
#' The method for handling functions returns the function unchanged. The method
#' for handling symbols evaluates the symbol, then attempts to apply itself to the
#' result. For calls, \code{lambda} creates a function that applies the new arguments,
#' along with the original arguments in the call, to the original call function.
#'
#' \code{lambda} attempts to parse a formula object - an object with syntax \code{LHS ~ RHS} -
#' by using the value on the left-hand side as the function arguments and the value on
#' the right-hand side as the function body. The argument on the left-hand side
#' is split across "." so that multiple arguments can be easily created. For example,
#' \code{lambda(x.y ~ sqrt(x + y))} creates \code{function(x, y) sqrt(x + y)}.
#' If the formula is only one-sided, then the formula is parsed similar to the
#' method in the \code{purrr} package; names that are prefixed by a "." are considered
#' the function arguments. The previous function could also be created using
#' \code{lambda(~sqrt(.x + .y))}.
#'
#' \code{lambda} is used in many of the higher-order functions in the \code{eList}
#' package. It can also be used in other functions so that users have a variety
#' of options in which they satisfy functional arguments.
#'
#' @return function
#' @export
#'
#' @examples
#' double2 <- lambda(x.y ~ 2*(x + y))
#' double2(5, 6)
#'
#' # alternatively, using 'dot' notation:
#' double2 <- lambda(~ 2*(.x + .y))
#'
#' # using a call with partial arguments
#' subcall <- substitute(round(digits=2))
#' round2 <- lambda(subcall)
#' round2(0.04393282)
#'
lambda <- function(x, ...) UseMethod("lambda")
#' @method lambda function
#' @export
lambda.function <- function(x, ...) x
#' @method lambda symbol
#' @export
lambda.symbol <- function(x, ...) lambda(eval(x), ...)
#' @method lambda name
#' @export
lambda.name <- function(x, ...) lambda(eval(x), ...)
#' @method lambda call
#' @importFrom stats as.formula
#' @export
lambda.call <- function(x, ...){
  if (x[[1]] == "~") return(lambda(as.formula(x)))
  structure(
    function(...){
      fun <- attr(sys.function(), "fun")
      args <- c(..., attr(sys.function(), "args"))
      do.call(fun, args)
    },
    fun = eval(x[[1]]),
    args = as.list(x)[-1]
  )
}
#' @method lambda formula
#' @export
lambda.formula <- function(x, ...){
  if (length(x) == 3){
    expr <- x[[3]]
    form <- unlist(strsplit(as.character(x[[2]]), "\\."))
  } else {
    form <- as.character(x)[2]
    reg  <- gregexpr("\\.[A-Za-z0-9_]+", form)[[1]]
    if (reg[1] == -1){
      expr <- str2lang(form)
      return(as.function(list(expr)))
    }
    expr <- str2lang(gsub("(\\.)(?=[A-z0-9_]+)", "", form, perl=TRUE))
    form <- sapply(seq_along(reg), function(i){
      substr(form, reg[i]+1, reg[i]+attr(reg, "match.length")[i]-1)
    })
  }
  args <- vector("list", length(form))
  for (i in seq_along(args)) args[[i]] <- substitute()
  names(args) <- form
  as.function(c(args, expr))
}
#' @method lambda list
#' @export
lambda.list <- function(x, ...) as.function(x)
