% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_top_moe_res.R
\name{get_top_moe_res}
\alias{get_top_moe_res}
\title{Get result for larger moe values}
\usage{
get_top_moe_res(res_moe, top = 1, list = FALSE)
}
\arguments{
\item{res_moe}{result from mr_moe().see more \code{\link[TwoSampleMR]{mr_moe}}}

\item{top}{number of rows for larger results}

\item{list}{the result is a list, which defaults to FALSE.}
}
\value{
a data frame or a list
}
\description{
Get the row with the higher moe value among multiple moe results
}
\examples{
res<-get_top_moe_res(res_moe)
}
