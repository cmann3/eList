#' 'for' Loop with Additional Features
#'
#' \code{ifor} evaluates an expression within a \code{for} loop, after applying
#' \code{\link{iter}} to the sequence. \code{ifor} also allows multiple indexes by
#' separating each variable name with a ".", such that \code{ifor(x, i.j, ...)}
#' is similar to \code{for (i,j in x) ...} if \code{for} loops accepted multiple
#' index values. See \code{\link{comprehension}} for more details. Assignment to
#' a variable outside of the function can be accomplished through \code{assign} or \code{<<-}.
#'
#' @param x sequence over which to loop
#' @param ind variable name whose values are updated each round in the loop. Separate names with "." to allow for multiple variables
#' @param expr expression that is evaluated each round within the loop
#'
#' @return \code{NULL} invisibly
#' @export
#'
#' @examples
#' ifor(i.j, zip(1:4, 0:3),{
#'  print(i+j)
#' })
#'
ifor <- function(ind, x, expr){
  x    <- iter(x)
  ind  <- substitute(ind)
  expr <- as.expression(substitute(expr))
  sym  <- unlist(strsplit(as.character(ind), ".", fixed=TRUE))
  narg <- length(sym)

  if (narg == 1){z <- paste0("for (", sym, " in x) ", expr)
  } else {
    z <- paste0("for (FORROUND in x){\n  ", paste(sapply(1:narg, function(i) if (sym[i] != ""){paste0(sym[i], " <- FORROUND[[", i, "]]")}), collapse="\n  "), "\n  ", expr, "\n}")
  }
  eval(parse(text=z))
  invisible(NULL)
}


