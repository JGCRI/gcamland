% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_scenario_info.R
\name{get_saved_share_weights}
\alias{get_saved_share_weights}
\title{get_saved_share_weights}
\usage{
get_saved_share_weights()
}
\value{
Share weights as a named vector
}
\description{
Read in share weights from a file
}
\details{
Returns a named vector of share weights, where names are the names of LandLeafs.
The resulting vector can be passed as an argument to the \code{\link{ScenarioInfo}} constructor,
to the \code{\link{update_scen_info}} function, or set directly to \code{mShareWeights} in a
\code{\link{ScenarioInfo}} object.
}
\author{
KVC December 2018
}
