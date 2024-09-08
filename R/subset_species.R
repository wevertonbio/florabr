#' Extract a subset of species from Flora e Funga do Brasil database
#' @description
#' Returns a data.frame with a subset of species from Flora e Funga do Brasil
#' database
#'
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function.
#' @param species (character) names of the species to be extracted from
#' Flora e Funga do Brasil database.
#' @param include_subspecies (logical) include subspecies?
#' Default = FALSE
#' @param include_variety (logical) include varieties of the species?
#' Default = FALSE
#' @param kingdom (character) The kingdom for filtering the dataset. It can be
#' "Plantae" or "Fungi". Default = "Plantae". To include both,
#' use c("Plantae", "Fungi")
#'
#' @return A data.frame with the selected species.
#' @usage subset_species(data, species,
#'                       include_subspecies = FALSE,
#'                       include_variety = FALSE,
#'                       kingdom = "Plantae")
#' @references
#' Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#' @export
#'
#' @examples
#' data("bf_data") #Load Flora e Funga do Brasil data
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
                           kingdom = "Plantae"){
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

  if(!(kingdom %in% unique(data$kingdom))) {
    stop(paste("kingdom not valid. The kingdoms availables are:\n",
               paste(unique(data$kingdom), collapse = ", ")))  }

  #Get binomial names of species
  Species <- get_binomial(species)

  #Start to filter...
  #kingdom
  d <- subset(data, data$kingdom %in% kingdom)

  #Taxon Rank
  if(!include_subspecies & !include_variety) {
    d <- subset(d, d$taxonRank == "Species") }
  if(include_subspecies & !include_variety) {
    d <- subset(d, d$taxonRank %in% c("Species", "Subspecies")) }
  if(!include_subspecies & include_variety) {
    d <- subset(d, d$taxonRank %in% c("Species", "Variety")) }
  if(include_subspecies & include_variety) {
    d <- subset(d, d$taxonRank %in% c("Species", "Subspecies", "Variety")) }

  #Create column with binomial name
  d$binomial <- get_binomial(d$species, include_subspecies = FALSE,
                             include_variety = FALSE)


  #Check if there is any species absent in d
  no_match <- setdiff(Species, unique(d$binomial))
  if(length(no_match) > 0) {
    warning(paste("Some species are absent of Flora e Funga do Brasil database\n",
                  "Check the species names using the check_names() function"))
  }

  d <- subset(d, d$binomial %in% Species)
  d$binomial <- NULL #Remove column
  return(d)
}
