% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nefsc.db.r
\name{nefsc.db}
\alias{nefsc.db}
\title{nefsc.db}
\usage{
nefsc.db(DS = "odbc.dump.redo")
}
\arguments{
\item{\code{DS}}{= the selection of data, consists of a full data dump from ODBC account through \code{odbc.dump}. Or individual data tables can be rebuilt (with \code{.redo}) or loaded as \code{uscat},\code{usdet},\code{usinf},\code{usstrata.area}}
}
\value{
saves or loads .rdata objects named \code{usinf}, \code{usdet}, \code{uscat}, \code{usstrat.area}
}
\description{
Pulls the offshore lobster data from NEFSC trawl surveys
}
\examples{
require(devtools)
load_all('E:/git/LobsterScience/bio.lobster') #to load from directory rather than package if modifying
nefsc.db(DS = 'odbc.dump.redo')
}
\author{
Adam Cook, \email{Adam.Cook@dfo-mpo.gc.ca}
}
