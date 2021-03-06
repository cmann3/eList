% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster.R
\name{auto_cluster}
\alias{auto_cluster}
\alias{close_cluster}
\title{Quickly Create a Cluster for Parallel Comprehension}
\usage{
auto_cluster(ncore = detectCores() - 1)

close_cluster(clust)
}
\arguments{
\item{ncore}{number of cores/nodes to use. If not specified, it attempts to detect the number of cores available and uses all but 1.}

\item{clust}{cluster to close the connection to}
}
\value{
an object of class c("SOCKcluster", "cluster")
}
\description{
A function to quickly create a cluster for use in parallel vector comprehensions.
Use \code{\link[parallel:makeCluster]{makeCluster}} from the \code{parallel} package
for greater control. It defaults to making a PSOCK cluster on Windows systems and
a Fork cluster on unix-based systems. \code{close_cluster} is a wrapper to
\code{\link[parallel:makeCluster]{stopCluster}}.
}
\section{Functions}{
\itemize{
\item \code{close_cluster}: close an open connection to a cluster
}}

\examples{
## Parallel vector comprehension
cluster <- auto_cluster(2)
Num(for (i in 1:1000) exp(sqrt(i)), clust=cluster)
close_cluster(cluster)

}
