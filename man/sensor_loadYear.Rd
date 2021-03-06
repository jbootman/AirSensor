% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sensor_loadYear.R
\name{sensor_loadYear}
\alias{sensor_loadYear}
\title{Load hourly-aggregated PurpleAir data for a month}
\usage{
sensor_loadYear(
  collection = "scaqmd",
  datestamp = NULL,
  timezone = "America/Los_Angeles"
)
}
\arguments{
\item{collection}{Name associated with the collection.}

\item{datestamp}{A date string in ymd order.}

\item{timezone}{Timezone used to interpret \code{datestamp}.}
}
\value{
An object of class "pa_timeseries".
}
\description{
A pre-generated \code{airsensor} object will be loaded for
the given month. Archived data for SCAQMD sensors go back to January, 2018.

The \code{datestamp} can must be in the following format:

\itemize{
\item{\code{"YYYYmm"}}
}

By default, the current month is loaded.

Each \code{airsensor} object contains data from a named collection of 
PurpleAir sensors.
}
\seealso{
\link{pat_createNew}
}
