% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lobster.db - original.r, R/lobster.db.r
\name{lobster.db}
\alias{lobster.db}
\title{lobster.db}
\usage{
lobster.db(DS = "complete.redo", pH = p)

lobster.db(DS = "complete.redo", pH = p)
}
\arguments{
\item{DS}{is the main switch that selects which data source to load or operate. Options for DS include 'complete','annual.landings','logs','logs41','logs41jonah','observer41','atSea','cris','port','vlog','fsrs','scallop','survey','annual.landings'.  Any of these arguements called as listed return the data object. To make the data file from scratch would require a 'XXXX.redo', where XXXX is the option listed above.}
}
\value{
Data objects that contain the data for use in further analyses.

Data objects that contain the data for use in further analyses.
}
\description{
This function is the main workhorse to pull data from databases and some initial filtering of data used in lobster stock assessments. Results are saved and can be reloaded using this function.

This function is the main workhorse to pull data from databases and some initial filtering of data used in lobster stock assessments. Results are saved and can be reloaded using this function.
}
\examples{
lobster.db('fsrs.redo') # makes the data objects for the FSRS data.
lobster.db('fsrs') #loads the object fsrs
lobster.db('fsrs.redo') # makes the data objects for the FSRS data.
lobster.db('fsrs') #loads the object fsrs
}
