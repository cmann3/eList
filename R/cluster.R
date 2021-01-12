#' Quickly Create a Cluster for Parallel Comprehension
#'
#' A function to quickly create a cluster for use in parallel vector comprehensions.
#' Use \code{\link[parallel:makeCluster]{makeCluster}} from the \code{parallel} package
#' for greater control. It defaults to making a PSOCK cluster on Windows systems and
#' a Fork cluster on unix-based systems. \code{close_cluster} is a wrapper to
#' \code{\link[parallel:makeCluster]{stopCluster}}.
#'
#' @param ncore number of cores/nodes to use. If not specified, it attempts to detect the number of cores available and uses all but 1.
#' @param clust cluster to close the connection to
#'
#' @return an object of class c("SOCKcluster", "cluster")
#' @importFrom parallel detectCores makePSOCKcluster makeForkCluster
#' @export
#'
#' @examples
#' ## Parallel vector comprehension
#' cluster <- auto_cluster(2)
#' Num(for (i in 1:1000) exp(sqrt(i)), clust=cluster)
#' close_cluster(cluster)
#'
auto_cluster <- function(ncore = detectCores()-1){
  os <- .Platform$OS.type
  if (.Platform$OS.type == "windows") return(makePSOCKcluster(ncore))
  makeForkCluster(ncore)
}

#' @describeIn auto_cluster close an open connection to a cluster
#' @importFrom parallel stopCluster
#' @export
close_cluster <- function(clust) stopCluster(clust)
