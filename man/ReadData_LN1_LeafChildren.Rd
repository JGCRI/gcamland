% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_data.R
\name{ReadData_LN1_LeafChildren}
\alias{ReadData_LN1_LeafChildren}
\title{ReadData_LN1_LeafChildren}
\usage{
ReadData_LN1_LeafChildren(
  aRegionName,
  aSubRegion,
  subregionData,
  aScenarioInfo
)
}
\arguments{
\item{aRegionName}{Region to read data for}

\item{aSubRegion}{Subregion to read data for}

\item{subregionData}{Subregion data}

\item{aScenarioInfo}{Scenario-related information, including names, logits, expectations}
}
\value{
Data on children of the land allocator
}
\description{
ReadData_LN1_LeafChildren
}
\details{
Read in the leaf children of LandNode1. That is
         read in information on children that only have one
         node above them.
}
\author{
KVC October 2017
}
