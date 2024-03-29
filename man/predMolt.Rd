% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predMolt.r
\name{predMolt}
\alias{predMolt}
\title{predMolt}
\usage{
predMolt(p, cl, doy = d, gdd = FALSE, sex)
}
\arguments{
\item{\code{p}}{:parameter list containing (at a minimum) area, doy (day of year), temp (temperature at doy)}

\item{\code{cl}}{: carapace width}
}
\value{
The predicted probability of moulting
}
\description{
Prediction of moulting based on day since last moult, size and can include temperature through a degree day metric
}
\examples{
require(devtools)
load_all('E:/git/LobsterScience/bio.lobster') #to load from directory rather than package if modifying
nefsc.db(DS = 'odbc.dump.redo')
}
\author{
Adam Cook, \email{Adam.Cook@dfo-mpo.gc.ca}
}
