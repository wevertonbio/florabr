#' Flora e Funga do Brasil database - Version 393.401
#'
#' @description
#'  A dataset containing a subset of the Flora e Funga do Brasil database
#'  (version 393.401)
#'
#' @usage data(bf_data)
#'
#' @format A \code{data.frame} with 50010 rows and 23 variables:
#' \describe{
#'   \item{species}{Species names}
#'   \item{scientificName}{Complete scientific name of the species}
#'   \item{acceptedName}{Accepted name of the species (NA when the name in
#'   species is already an accepted name)}
#'   \item{kingdom}{Kingdom to which species belongs (Plantae or Fungi)}
#'   \item{group}{Major group to which species belongs (Angiosperms,
#'   Gymnosperms, Ferns and Lycophytes, Bryophytes, and Algae)}
#'   \item{subgroup}{Subgroup to which species belongs. Only available for
#'   Bryophytes (Mosses, Hornworts, and Liverworts)}
#'   \item{phylum}{Phylum to which species belongs}
#'   \item{class}{Class to which species belongs}
#'   \item{order}{Order to which species belongs}
#'   \item{family}{Family to which species belongs}
#'   \item{genus}{Genus to which species belongs}
#'   \item{lifeForm}{Life form of the species (e.g: Tree, Herb, Shrub, etc.)}
#'   \item{habitat}{Habitat type of the species (e.g., Terrestrial, Rupicolous,
#'   Epiphytic, etc.)}
#'   \item{biome}{Biomes with confirmed occurrences of the species}
#'   \item{states}{Federal states with confirmed occurrences of the species}
#'   \item{vegetation}{Vegetation types with confirmed occurrences of the
#'   species}
#'   \item{origin}{Indicates whether the species is Native, Naturalized, or
#'   Cultivated in Brazil}
#'   \item{endemism}{Indicates whether the species is Endemic or Non-endemic to
#'   Brazil}
#'   \item{taxonomicStatus}{Indicates the level of recognition and acceptance
#'   of the species (Accepted or Synonym)}
#'   \item{nomenclaturalStatus}{Indicates the legitimacy and validity of the
#'   species name (Correct, Illegitimate, Uncertain_Application, etc.)}
#'   \item{vernacularName}{Locally or culturally used name for the species}
#'   \item{taxonRank}{Taxonomic rank (Species, Genus, Family, Order, etc). This
#'   data contains only Species}
#'   \item{id}{Unique id for species}
#' }
#' @references
#' Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
"bf_data"

#' SpatVector of the federal states of Brazil
#'
#' @description
#'  A simplified and packed SpatVector of the polygons of the federal states of
#'  Brazil. The spatial data was originally obtained from
#'  \code{geobr::read_state}. Borders have been simplified by removing vertices
#'  of borders using \code{terra::simplifyGeom}. It's necessary unpack the
#'  Spatvectos using \code{terra::unwrap}
#'
#'  @usage data(states)
#'  states <- terra::unwrap(states)
#'
#' @format A \code{SpatVector} with 27 geometries and 3 attributes:
#' \describe{
#'   \item{abbrev_state}{State acronym}
#'   \item{name_state}{State's full name}
#'   \item{name_region}{The region to which the state belongs}
#'   }
"states"

#' SpatVector of the biomes of Brazil
#'
#' @description
#'  A simplified and packed SpatVector of the polygons of the biomes present in
#'  Brazilian territory. The spatial data was originally obtained from
#'  \code{geobr::read_biomes}. Borders have been simplified by removing vertices
#'   of borders using \code{terra::simplifyGeom}. It's necessary unpack the
#'   Spatvectos using \code{terra::unwrap}
#'
#'  @usage data(biomes)
#'  biomes <- terra::unwrap(biomes)
#'
#' @format A \code{SpatVector} with 6 geometries and 1 attribute:
#' \describe{
#'   \item{name_biome}{The name of the biome (Amazon, Caatinga, Cerrado,
#'    Atlantic_Forest, Pampa, and Pantanal)}
#'   }
"biomes"

#' SpatVector of the Brazil's national borders
#'
#' @description
#'  A simplified and packed SpatVector of the Brazil's national borders. The
#'  spatial data was originally obtained from \code{geobr::read_country}.
#'  Borders have been simplified by removing vertices of borders using
#'  \code{terra::simplifyGeom}. It's necessary unpack the Spatvectos using
#'  \code{terra::unwrap}
#'
#'  @usage data(brazil)
#'  brazil <- terra::unwrap(brazil)
#'
#' @format A \code{SpatVector} with 1 geometry and 0 attribute
"brazil"

#' Records of plant species
#'
#' @description
#'  A dataset containing records of 7 plant species downloaded from GBIF. The
#'  records were obtained with \code{plantR::rgbif2}
#'
#' @usage data(occurrences)
#'
#' @format A \code{data.frame} with 1521 rows and 3 variables:
#' \describe{
#'   \item{species}{Species names (Araucaria angustifolia, Abatia americana,
#'   Passiflora edmundoi, Myrcia hatschbachii, Serjania pernambucensis, Inga
#'   virescens, and Solanum restingae)}
#'   \item{x}{Longitude}
#'   \item{y}{Latitude}
#' }
#' @references GBIF, 2024. florabr R package: Records of plant species. https://doi.org/10.15468/DD.QPGEB7
"occurrences"
