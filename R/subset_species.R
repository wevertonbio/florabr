#' Extract a subset of species from Brazilian Flora 2020 database
#' @description
#' Returns a data.frame with a subset of species from Brazilian Flora 2020
#' database
#'
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function.
#' @param species (character) names of the species to be extracted from
#' Brazilian Flora database.
#' @param include_subspecies (logical) include subspecies?
#' Default = FALSE
#' @param include_variety (logical) include varieties of the species?
#' Default = FALSE
#' @param Kingdom (character) The Kingdom for filtering the dataset. It can be
#' "Plantae" or "Fungi". Default = "Plantae". To include both,
#' use c("Plantae", "Fungi")
#'
#' @return A data.frame with the selected species.
#' @usage subset_species(data, species,
#'                       include_subspecies = FALSE,
#'                       include_variety = FALSE,
#'                       Kingdom = "Plantae")
#' @references
#' Brazilian Flora 2020. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#' @export
#'
#' @examples
#' data("bf_data") #Load Brazilian Flora data
#' #Species to extract from database
#' spp <- c("Araucaria angustifolia", "Adesmia paranensis")
#' spp_bf <- subset_species(data = bf_data, species = spp,
#'                       include_subspecies = FALSE,
#'                       include_variety = FALSE)
#' spp_bf
subset_species <- function(data,
                           species,
                           include_subspecies = FALSE,
                           include_variety = FALSE,
                           Kingdom = "Plantae"){
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

  if (!is.logical(include_subspecies)) {
    stop(paste0("Argument include_subspecies must be logical, not ",
                class(include_subspecies)))
  }

  if (!is.logical(include_variety)) {
    stop(paste0("Argument include_variety must be logical, not ",
                class(include_variety)))
  }

  if(!(Kingdom %in% unique(data$kingdom))) {
    stop(paste("Kingdom not valid. The Kingdoms availables are:\n",
               paste(unique(data$kingdom), collapse = ", ")))  }

  #Get binomial names of species
  Species <- get_binomial(species)

  #Start to filter...
  #Kingdom
  d <- subset(data, data$kingdom %in% Kingdom)

  #Taxon Rank
  if(!include_subspecies & !include_variety) {
    d <- subset(d, d$taxonRank == "Species") }
  if(include_subspecies & !include_variety) {
    d <- subset(d, d$taxonRank %in% c("Species", "Subspecies")) }
  if(!include_subspecies & include_variety) {
    d <- subset(d, d$taxonRank %in% c("Species", "Variety")) }
  if(include_subspecies & include_variety) {
    d <- subset(d, d$taxonRank %in% c("Species", "Subspecies", "Variety")) }

  #Check if there is any species absent in d
  no_match <- setdiff(Species, unique(d$species))
  if(length(no_match) > 0) {
    warning(paste("Some species are absent of Brazilian Flora database\n",
                  "Check the species names using the check_names() function"))
  }

  d <- subset(d, d$species %in% Species)
  return(d)
}
