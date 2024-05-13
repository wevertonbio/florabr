#' Get available attributes to filter species
#'
#' @description
#' This function displays all the options available to filter species by its
#' characteristics
#'
#'
#' @param data (data.frame) a data.frame imported with the
#' \code{\link{load_florabr}} function or a data.frame generated with the
#'  \code{\link{select_species}} function.
#' @param attribute (character) the type of characteristic. See detail to see
#' the options.
#' @param kingdom (character) the kingdom to which the species belong. It can
#' be "Plantae" or "Fungi". Default = "Plantae".
#'
#' @details
#' The attribute argument accepts the following options: group, subgroup,
#' family, lifeform, habitat, vegetation, origin, endemism, biome, states,
#' taxonomicstatus or nomenclaturalstatus. These options represent different
#' characteristics of species that can be used for filtering.
#'
#' @return a data.frame with the available options to use in the
#' \code{\link{select_species}} function.
#'
#' @usage get_attributes(data, attribute, kingdom = "Plantae")
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
#' get_attributes(data = bf_data, kingdom = "Plantae", attribute = "Biome")
#' # Get available life forms to filter species
#' get_attributes(data = bf_data, kingdom = "Plantae", attribute = "lifeForm")
#' # Get available states to filter species
#' get_attributes(data = bf_data, kingdom = "Plantae", attribute = "States")

get_attributes <- function(data, attribute,
                           kingdom = "Plantae") {

  if (missing(data)) {
    stop("Argument data is not defined")
  }

  #Change specified attribute to lowercase
  attrib <- tolower(attribute)

  #Correct attribute
  attrib[which(attrib == "lifeform")] <- "lifeForm"
  attrib[which(attrib == "taxonomicstatus")] <- "taxonomicStatus"
  attrib[which(attrib == "nomenclaturalstatus")] <- "nomenclaturalStatus"

  #Change kingdom - First letter to upper case
  kingdom <- firstup(kingdom)

  if (missing(attribute)) {
    stop("Argument attribute is not defined. Valid attributes:
    group, subgroup, phylum, class, order, family, lifeForm, habitat,
    vegetation, origin, endemism, biome, states, taxonomicStatus or
         nomenclaturalStatus")
  }

  #Check classes
  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ", class(data)))
  }

  if (!is.character(attribute)) {
    stop(paste0("Argument attribute must be a character, not ",
                class(attribute)))
  }

  if (!(kingdom %in% c("Plantae", "Fungi"))) {
    stop("Argument kingdom must be 'Plantae' or 'Fungi'")
  }


  if(!(attrib %in% c("group", "subgroup", "family", "lifeForm", "habitat",
                    "vegetation", "origin", "endemism", "biome", "states",
                    "taxonomicStatus", "nomenclaturalStatus", "phylum", "class",
                    "order"))) {
    stop("Informed attribute is not valid! Valid attributes:
    group, subgroup, phylum, class, order, family, lifeForm, habitat,
    vegetation, origin, endemism, biome, states, taxonomicStatus or
    nomenclaturalStatus")
  }

  #Get unique attributes
  d <- data[data$kingdom == kingdom,]
  d_at <- d[,attrib]
  at <- unique(unlist(strsplit(d_at, ";")))
  #Save in datafra,e
  att_f <- data.frame(atrib = sort(at))
  colnames(att_f) <- attrib

  return(att_f)
}


