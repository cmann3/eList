#' @export
`[.Comprehension` <- function(COMPREHENSION, obj, ...){
  mapfun <- attr(COMPREHENSION, "mapfun")
  fun  <- attr(COMPREHENSION, "fun")
  ret <- eval(parse(text=.parse_for(substitute(obj), mapfun=mapfun, ...)))
  if (is.null(fun)) return(ret)
  fun(ret)
}


#' @export
`[.VecGen` <- function(VECTORGENERATOR, ..., type=Vec, simplify=TRUE){
  dots <- eval(substitute(alist(...)))
  mapfun <- attr(type, "mapfun")
  fun <- attr(type, "fun")
  l <- lapply(dots, function(i){
    e <- eval(parse(text=.parse_for(i, mapfun=mapfun)))
    if (is.null(fun)) return(e)
    fun(e)
  })
  if (length(dots) == 1) return(l[[1]])
  if (simplify) return(Reduce(c, l))
  l
}


