#' Retrieve synonyms for species
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function
#' @param species (character) names of the species
#'
#' @return A data.frame containing unique synonyms of the specified species
#' along with relevant information on taxonomic and nomenclatural statuses.
#' @usage get_synonym(data, species)
#' @export
#' @references
#' Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#' @examples
#' data("bf_data") #Load Flora e Funga do Brasil data
#' #Species to extract synonyms
#' spp <- c("Araucaria angustifolia", "Adesmia paranensis")
#' spp_synonyms <- get_synonym(data = bf_data, species = spp)
#' spp_synonyms
#'
get_synonym <- function(data, species){

  if (missing(data)) {
    stop("Argument data is not defined")
  }

  #Check classes
  if (!is.data.frame(data)) {
    stop(paste0("Argument data must be a data.frame, not ", class(data)))
  }

  if (!is.character(species)) {
    stop(paste0("Argument species must be a character, not ", class(species)))
  }

  #Check if there is any species absent in d
  no_match <- setdiff(species, unique(data$species))
  if(length(no_match) > 0 & length(no_match) < length(species)) {
    warning(paste("Some species are absent of Flora e Funga do Brasil database\n",
                  "Check the species names using the check_names() function"))
  }
  #Get match
  order <- setdiff(species, no_match)

  res <- unique(data[which(data$acceptedName %in% species), c("species", "acceptedName",
                                                  "taxonomicStatus",
                                                  "nomenclaturalStatus")])

  if(nrow(res) > 0) {
  #Reorder
  res <- res[order(match(res$acceptedName, order)), ]

  #Change name of the column
  colnames(res)[1] <- "synonym"

  #Remove accepted names
  res <- subset(res, !(res$synonym %in% species))

  return(res) } else {
    warning(paste("All specified species are absent of Flora e Funga do Brasil
                  database\n",
               "Check the species names using the check_names() function"))
  }

}
