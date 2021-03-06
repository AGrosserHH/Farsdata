% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{make_filename}
\alias{make_filename}
\title{Make data file name}
\usage{
make_filename(year)
}
\arguments{
\item{year}{A string or an integer with the input}
}
\value{
This function returns a string with the data file name for a given
  year, and the file path within the package.
}
\description{
Make .csv data file name related to the given year. There is no check if the file is available.
}
\examples{
\dontrun{
make_filename(2013)
}

}
