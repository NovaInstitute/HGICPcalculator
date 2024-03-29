% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateER.R
\name{calculateER}
\alias{calculateER}
\title{calculateER}
\usage{
calculateER(BE, PE, LE, format = NULL, web3 = FALSE)
}
\arguments{
\item{BE}{tibble from calculateE(  ..., outcome = "BE")}

\item{PE}{tibble from calculateE(  ..., outcome = "PE")}

\item{LE}{tibble from calculateE(  ..., outcome = "LE")}

\item{format}{Character. Output format if called as API. one of "json", "csv", "rds", "htmlTable"}

\item{web3}{Logical. Return web3storage address or not}
}
\value{
data.frame
}
\description{
calculateER
}
