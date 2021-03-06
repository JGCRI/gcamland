% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/land_node.R
\name{LandNode_calcLandShares}
\alias{LandNode_calcLandShares}
\title{LandNode_calcLandShares}
\usage{
LandNode_calcLandShares(aLandNode, aChoiceFnAbove, aPeriod)
}
\arguments{
\item{aLandNode}{Land node to perform calculations on}

\item{aChoiceFnAbove}{The discrete choice function from the level above.}

\item{aPeriod}{Period.}
}
\description{
LandNode_calcLandShares
}
\details{
Uses the logit formulation to calculate the share
         of land allocated to a particular land type. A node's share
         is based on its profit rate and distribution parameter.
         A node's profit rate is NOT the weighted average of its
         childrens' profit rates but is based on the J. Clarke and Edmonds
         Logit paper and uses the scaled profits of the child nodes and leafs.
}
\author{
KVC September 2017
}
