% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_florabR.R
\name{load_florabr}
\alias{load_florabr}
\title{Load Brazilian Flora database}
\usage{
load_florabr(data_dir, data_version = "Latest_available",
type = "short")
}
\arguments{
\item{data_dir}{(character) the same directory used to save the data
downloaded from Brazilian Flora 2020 using the \link{get_florabr} function.}

\item{data_version}{(character) the version of Brazilian Flora database to
be loaded. It can be "Latest_available", which will load the latest version
available; or another specified version, for example "393.364".
Default = "Latest_available".}

\item{type}{(character) it determines the number of columns that will be loaded. It can be "short" or "complete". Default = "short". See details.}
}
\value{
A data.frame with the specified version (Default is the latest available) of the Brazilian Flora database. This data.frame is necessary to run most of the functions of the package.
}
\description{
Load Brazilian Flora database
}
\details{
The parameter type accepts two arguments. If type = short, it will load a data.frame with the 19 columns needed to run the other functions of the package: species, scientificName, acceptedName, kingdom, Group, Subgroup, family, genus, lifeForm, habitat, Biome, States, vegetationType, Origin,Endemism, taxonomicStatus, nomenclaturalStatus, vernacularName, and taxonRank.
If type = complete, it will load a data.frame with all 39 variables available in Brazilian Flora database.
}
\examples{
\dontrun{
dir.create("brazilianflora") #Create a directory to save data
my_dir <- "brazilianflora" #Set directory to save data
#Download, merge and save data
get_florabr(output_dir = my_dir, data_version = "latest", overwrite = TRUE)
#Load data
df <- load_florabr(data_dir = my_dir, data_version = "Latest_available",
type = "short")
}
}
\references{
Brazilian Flora 2020. Jardim Botânico do Rio de Janeiro. Available at:
http://floradobrasil.jbrj.gov.br/
}