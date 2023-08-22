#' Get available attributes to filter species
#'
#' @description
#' This function displays all the options available to filter species by its characteristics
#'
#'
#' @param data (data.frame) a data.frame imported with the
#' \code{\link{load_florabr}} function or a data.frame generated with the
#'  \code{\link{select_species}} function.
#' @param attribute (character) the type of characteristic. See detail to see the options.
#' @param Kingdom (character) the kingdom to which the species belong. It can be "Plantae" or "Fungi". Default = "Plantae".
#'
#' @details
#' The attribute argument accepts the following options: Group, SubGroup, family, lifeForm, habitat, vegetationType, Origin, Endemism, Biome, States, taxonomicStatus or nomenclaturalStatus". These options represent different characteristics of species that can be used for filtering.
#'
#' @return a data.frame with two columns. The first column provides the available options in English. Use this options in the \code{\link{select_species}} function. The second columns provides the options in Portuguese.
#'
#' @usage get_attributes(data, attribute, Kingdom = "Plantae")
#'
#' @export
#'
#' @references
#' Brazilian Flora 2020. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#'
#' @examples
#' data("bf_data") #Load Brazilian Flora data
#' # Get available biomes to filter species
#' get_attributes(data = bf_data, Kingdom = "Plantae", attribute = "Biome")
#' # Get available life forms to filter species
#' get_attributes(data = bf_data, Kingdom = "Plantae", attribute = "lifeForm")
#' # Get available states to filter species
#' get_attributes(data = bf_data, Kingdom = "Plantae", attribute = "States")

get_attributes <- function(data = NULL, attribute = NULL,
                           Kingdom = "Plantae") {

  if (missing(data)) {
    stop("Argument data is not defined")
  }

  if (missing(attribute)) {
    stop("Argument attribute is not defined. Valid attributes:
    Group, SubGroup, family, lifeForm, habitat, vegetationType, Origin, Endemism,
    Biome, States, taxonomicStatus or nomenclaturalStatus")
  }

  #Check classes
  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ", class(data)))
  }

  if (!is.character(attribute)) {
    stop(paste0("Argument attribute must be a character, not ", class(attribute)))
  }

  if (!(Kingdom %in% c("Plantae", "Fungi"))) {
    stop("Argument Kingdom must be 'Plantae' or 'Fungi'")
  }


  atrib <- attribute

  if(!(atrib %in% c("Group", "Subgroup", "family", "lifeForm", "habitat", "vegetationType", "Origin","Endemism", "Biome", "States",
                  "taxonomicStatus", "nomenclaturalStatus"))) {
    stop("Informed attribute is not valid! Valid attributes:
    Group, SubGroup, family, lifeForm, habitat, vegetationType, Origin, Endemism,
    Biome, States, taxonomicStatus or nomenclaturalStatus")
  }

  #Get unique attributes
  d <- subset(data, data$kingdom == Kingdom)
  d_at <- d[,atrib]
  at <- unique(unlist(strsplit(d_at, ";")))

  if(atrib != c("family")) {
  #Get attribute translations
  Attributes <- florabr::Attributes
  Attributes <- Attributes[[atrib]]

  #Subset
  att_f <- subset(Attributes, Attributes[,1] %in% at) }

  if(atrib == "family"){
    att_f <- data.frame(family = sort(at))
  }

  return(att_f)
}


