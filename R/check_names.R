#' Check species names
#'
#' @description check_names checks if the species names are correct and searches
#'  for suggestions if the name is misspelled or not found in the Flora e Funga
#'  do Brasil database
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function.
#' @param species (character) names of the species to be checked.
#' @param max_distance (numeric) Maximum distance (as a fraction) allowed for
#' searching suggestions when the name is misspelled. It can be any value
#' between 0 and 1. The higher the value, the more suggestions are returned.
#' For more details, see \code{\link[base:agrep]{agrep}}. Default = 0.1.
#' @param kingdom (character) the kingdom to which the species belong. It can
#' be "Plantae" or "Fungi". Default = "Plantae".
#'
#' @return a data.frame with the following columns:
#' - input_name: the species names informed in species argument
#' - Spelling: indicates if the species name is Correct (a perfect match with a
#' species name in the Flora e Funga do Brasil), Probably_incorrect
#' (partial match), or Not_found (no match with any species).
#' - Suggested name: If Spelling is Correct, it is the same as the input_name.
#' If Spelling is Probably_correct, one or more suggested names are listed,
#' found according to the maximum distance. If Spelling is "Not_found", the value
#' is NA.
#' - Distance: The integer Levenshtein edit distance. It represents the number
#' of single-character edits (insertions, deletions, or substitutions) required
#' to transform the input_name into the Suggested_name.
#' - taxonomicStatus: the taxonomic status of the species name ("Accepted" or
#' "Synonym").
#' - nomenclaturalStatus: the nomenclatural status of the species name. This
#' information is not available for all species.
#' - acceptedName: If the species name is not accepted or incorrect, the
#' accepted name of the specie. If the species name is accepted and correct,
#' the same as input_name and Suggested_name.
#' - family: the family of the specie.
#' @usage check_names(data, species, max_distance = 0.1, kingdom = "Plantae")
#' @export
#' @importFrom utils adist
#' @references
#' Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#' @examples
#' data("bf_data", package = "florabr")
#' spp <- c("Butia cattarinensis", "Araucaria angustifolia")
#' check_names(data = bf_data, species = spp)

check_names <- function(data, species, max_distance = 0.1,
                        kingdom = "Plantae"){
  if (missing(data)) {
    stop("Argument data is not defined")
  }

  if (missing(species)) {
    stop("Argument species is not defined")
  }

  #Check classes
  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ", class(data)))
  }

  if (!is.character(species)) {
    stop(paste0("Argument species must be a character, not ", class(species)))
  }

  if (!is.numeric(max_distance)) {
    stop(paste0("Argument max_distance must be numeric, not ",
                class(max_distance)))
  }
  if(max_distance < 0 | max_distance > 1) {
    stop(paste0("Argument max_distance must be a numeric value between 0 and 1"
                ))
  }

  #Change kingdom - First letter to upper case
  kingdom <- firstup(kingdom)

  if(!(kingdom %in% c("Plantae", "Fungi"))) {
    stop(paste0("Argument kingdom must be 'Plantae' or 'Fungi'"))
  }

  #Continue function...
  data <- subset(data, data$kingdom == kingdom & data$taxonRank == "Species")
  all_sp <- unique(data$species)

  #Get species with correct and incorrect spell
  species <- get_binomial(species) #Get binomial names
  sp_in <- subset(species, species %in% all_sp)
  sp_in_df <- data.frame(input_name = sp_in, Suggested_name = sp_in)
  sp_out <- subset(species, !(species %in% all_sp))

  sp_l <- lapply(seq_along(sp_out), function(i){
    sp_agrep <- agrep(sp_out[i], all_sp, value = TRUE,
                    max.distance = max_distance)
    if(length(sp_agrep) == 0) {sp_agrep <- NA}
    sp_df <- data.frame(input_name = sp_out[i], Suggested_name = sp_agrep)
    return(sp_df)
    })
  spp <- do.call("rbind", sp_l)
  spp <- rbind(sp_in_df, spp)

  #Calculate distance
  spp$Distance <- vapply(seq_len(nrow(spp)), FUN.VALUE = double(1),
                         function(i){
    Distance <- adist(spp[i,1], spp[i,2])
    })

  #Create columns
  spp$Spelling <- ifelse(is.na(spp$Distance), "Not_found",
                         ifelse(spp$Distance > 0, "Probably_incorrect",
                                ifelse(spp$Distance == 0, "Correct", NA)))
  #Get information about Family, taxonomic and nomenclatural status
  d_info <- subset(data, species %in% spp$Suggested_name)
  d_info <- d_info[c("species", "taxonomicStatus", "nomenclaturalStatus",
                     "acceptedName", "family")]
  #Merge info
  spp_info <- merge(spp, d_info, by.x ="Suggested_name", by.y = "species",
                    all = TRUE, sort = FALSE)
  #Organize columns
  spp_info <- spp_info[, c("input_name", "Spelling", "Suggested_name",
                           "Distance", "taxonomicStatus",
                           "nomenclaturalStatus", "acceptedName", "family")]
  spp_info$acceptedName[which(spp_info$taxonomicStatus == "Accepted" &
                        is.na(spp_info$acceptedName))] <-
    spp_info$Suggested_name[which(spp_info$taxonomicStatus == "Accepted" &
                              is.na(spp_info$acceptedName))]

  return(spp_info)
}

