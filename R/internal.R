.charparse <- function(a){
  if (is.character(a)) return(paste0("\"", a, "\""))
  as.character(as.expression(a))
}

.symparse <- function(x){
  if (grepl(",", x)) return(gsub(" ", "", strsplit(x, ",", fixed=TRUE)[[1]]))
  return(strsplit(x, ".", fixed=TRUE)[[1]])
}


#' @export
.compfun <- function(loop, ...){
  mapfun <- attr(sys.function(), "mapfun")
  fun  <- attr(sys.function(), "fun")
  ret <- eval(parse(text=.parse_for(substitute(loop), mapfun=mapfun, ...)))
  if (is.null(fun)) return(ret)
  fun(ret)
}

#' @export
.sumfun <- function(loop, ..., na.rm=FALSE, collapse=""){
  mapfun <- attr(sys.function(), "mapfun")
  fun  <- attr(sys.function(), "fun")
  ret <- eval(parse(text=.parse_for(substitute(loop), mapfun=sapply, ...)))
  fun(ret, na.rm=na.rm, collapse=collapse)
}

.parse_for <- function(loop, nround = 0, mapfun=lapply, ...){
  if (class(loop) == "for"){
    start <- paste0("new_list <- mapfun(iter(", as.expression(loop[[3]]), "), ")

    # Symbols passed to Function
    sym <- .symparse(as.character(loop[[2]]))
    #sym <- unlist(strsplit(as.character(loop[[2]]), ".", fixed=TRUE))
    narg <- length(sym)
    var <- paste0("LAPPLYROUND", nround)
    if (narg > 1){
      fun <- c(paste0("function(", var, ")"), "{",
               sapply(1:narg, function(i) if (sym[i] != ""){paste0(sym[i], " <- ", var, "[[", i, "]]")}))
    } else {fun <- c(paste0("function(", var, ")"), "{", paste0(sym[1], " <- ", var))}
    fun2 <- "EXPRPARSE <- substitute({"

    # Assignment
    if (is.call(loop[[4]]) && as.character(loop[[4]][[1]]) == "="){
      body <- .parse_for(loop[[4]][[3]], nround = nround+1, mapfun=mapfun)
      assign <- c(paste0("names(new_list) <- sapply(", as.expression(loop[[3]]), ", "), fun, .charparse(loop[[4]][[2]]), "})")

    } else {
      body <- .parse_for(loop[[4]], nround = nround+1, mapfun=mapfun)
      assign <- ""
    }

    # Remove Nulls
    final <- "null.omit(new_list)"

    return(c(start, fun, fun2, body, "})", "eval(EXPRPARSE)", "})", assign, final))
  } else if (class(loop) == "if"){
    start <- c("if (", loop[[2]], ") {")
    mid <- .parse_for(loop[[3]], nround = nround+1, mapfun=mapfun)
    if (length(loop) > 3){
      final <- c("} else {", .parse_for(loop[[4]], nround = nround+1, mapfun=mapfun), "}")
    } else {final <- "}"}
    return(c(start, mid, final))
  } else if (class(loop) == "{"){
    #if (length(loop) > 1) return(sapply(2:length(loop), function(i) .parse_for(loop[[i]], nround=nround+1, mapfun=mapfun)))
    #return("")
    if (length(loop) > 1) return(c("{", Reduce(c, sapply(2:length(loop), function(i) as.character(.parse_for(loop[[i]], nround=nround+1, mapfun=mapfun)))), "}"))
    return(c("{", "}"))
  } else if (class(loop) == "<-"){
    return(c(paste0(paste(as.character(loop[[2]]), collapse="\n"), " <- "), as.character(.parse_for(loop[[3]], nround+1, mapfun))))
  } else if (is.character(loop)) {return(paste0("\"", loop, "\""))}
  ch <- as.character(as.expression(loop))
  ### Below might not be needed any more ...
  if (ch[1] == "{") ch <- ch[-1]
  if (ch[length(ch)] == "}") ch <- ch[-length(ch)]
  return(ch)
}


#' @export
.save_names <- function(x, fun){
  nm <- names(x)
  x <- fun(x)
  names(x) <- nm
  return(x)
}
