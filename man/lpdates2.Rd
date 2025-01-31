% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lpdates2.R
\name{lpdates2}
\alias{lpdates2}
\title{Pulling Dates}
\usage{
lpdates2(x, year1=1981, date1="05-01", date2="06-01", date3="08-31", date4="10-31")
}
\arguments{
\item{x}{Date vector}

\item{year1}{The year the Date vector starts on}

\item{date1}{Start index to find first onset date}

\item{date2}{End index to find first onset date}

\item{date3}{Start index to find second onset date}

\item{date4}{End index to find second onset date}
}
\value{
A vector of indexes that correspond to the four dates every year, with respect to the start date.

NOTE: It would be significantly easier to save the output as a variable called 'dated', especially when using rasters so that you do not have to create an intermediate function.
}
\description{
This function pulls the dates that correspond with the Mid Summer Drought, and is used in conjuction with the MSD function.
}
\examples{
dates<-seq(from = as.Date("1981-01-01"), to = as.Date("2020-12-31"), by = "day") 
lpdates2(dates, year1=1981, date1="05-01", date2="06-01", date3="08-31", date4="10-31")

}
