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
#' @param attribute (character) the type of characteristic. Accept more than one
#' option. See detail to see the options.
#'
#' @details
#' The attribute argument accepts the following options: kingdom, group,
#' subgroup, phylum, class, order, family, lifeform, habitat, vegetation,
#' origin, endemism, biome, states, taxonomicstatus or nomenclaturalstatus.
#' These options represent different characteristics of species that can be used
#' for filtering.
#'
#' @return a list of data.frames with the available options to use in the
#' \code{\link{select_species}} function.
#'
#' @usage get_attributes(data, attribute)
#'
#' @export
#'
#' @references
#' Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#'
#' @examples
#' data("bf_data") #Load Flora e Funga do Brasil data
#' # Get available biomes, life forms and states to filter species
#' d <- get_attributes(data = bf_data,
#'                     attribute = c("biome", "lifeform", "states"))

get_attributes <- function(data, attribute) {

  if (missing(data)) {
    stop("Argument data is not defined")
  }

  #Change specified attribute to lowercase
  attrib <- tolower(attribute)

  #Correct attribute
  attrib[which(attrib == "lifeform")] <- "lifeForm"
  attrib[which(attrib == "taxonomicstatus")] <- "taxonomicStatus"
  attrib[which(attrib == "nomenclaturalstatus")] <- "nomenclaturalStatus"

  if (missing(attribute)) {
    stop("Argument attribute is not defined. Valid attributes:
    kingdom, group, subgroup, phylum, class, order, family, lifeForm, habitat,
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


  #Check attributes
  no_valid <- setdiff(attrib, c("kingdom", "group", "subgroup", "family", "lifeForm", "habitat",
                                "vegetation", "origin", "endemism", "biome", "states",
                                "taxonomicStatus", "nomenclaturalStatus", "phylum", "class",
                                "order"))

  if(length(no_valid) > 0) {
    stop(paste0("The informed attributes is/are not valid:\n",
                paste(no_valid, collapse = "\n")),
    "\nValid attributes:
    kingdom, group, subgroup, phylum, class, order, family, lifeForm, habitat,
    vegetation, origin, endemism, biome, states, taxonomicStatus or
    nomenclaturalStatus")
  }

  #Get unique attributes
  d_l <- lapply(attrib, function(i){
  d_at <- data[,i]
  at <- unique(unlist(strsplit(d_at, ";")))
  #Save in dataframe
  att_f <- data.frame(var = sort(at))
  colnames(att_f) <- i

  #If states, return name of the states in another column
  if(i == "states") {
  s_df <- data.frame(
    states = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT",
              "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO",
              "RR", "SC", "SP", "SE", "TO"),
    states_name = c("Acre", "Alagoas", "Amapa", "Amazonas", "Bahia", "Ceara",
                   "Distrito Federal", "Espirito Santo", "Goias", "Maranhao",
                   "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Para",
                   "Paraiba", "Parana", "Pernambuco", "Piaui", "Rio de Janeiro",
                   "Rio Grande do Norte", "Rio Grande do Sul", "Rondonia",
                   "Roraima", "Santa Catarina", "Sao Paulo", "Sergipe",
                   "Tocantins"))
  att_f <- s_df[which(s_df$states %in% att_f$states),] }

  return(att_f)
  })
  names(d_l) <- attrib

  return(d_l)
}


