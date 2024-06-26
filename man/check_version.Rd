% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_version.R
\name{check_version}
\alias{check_version}
\title{Check if you have the latest version of Flora e Funga do Brasil data
available}
\usage{
check_version(data_dir)
}
\arguments{
\item{data_dir}{the directory where the data should be located.}
}
\value{
A message informing whether you have the latest version of Flora e
Funga do Brasil available in the data_dir
}
\description{
This function checks if you have the latest version of the Flora e Funga do
Brasil data available in a specified directory.
}
\examples{
#Check if there is a version of Flora e Funga do Brasil data available in the
#current directory
check_version(data_dir = getwd())
}
