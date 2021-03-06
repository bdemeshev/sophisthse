% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sophisthse.R
\name{series2tables}
\alias{series2tables}
\title{Get table name from time series name}
\usage{
series2tables(ts_names)
}
\arguments{
\item{ts_names}{character vector of time series or table names}
}
\value{
character vector of corresponding table names
}
\description{
Get table name from time series name
}
\details{
On sophist.hse.ru time series are stored in tables. The package download whole tables
and not individual time series. When user requests time series we need to know
corresponding table name.
}
\examples{
series2tables("M2_Y")
}
