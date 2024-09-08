#' Check species names
#'
#' @description `check_names` checks if the species names are correct and searches
#'  for suggestions if the name is misspelled or not found in the Flora e Funga
#'  do Brasil database
#'
#' `match_names` finds approximate matches to the specified pattern (species)
#' within each element of the string `x` (species_to_match). It is used internally
#' by `check_names`.
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function.
#' @param species (character) names of the species to be checked.
#' @param max_distance (numeric) Maximum distance (as a fraction) allowed for
#' searching suggestions when the name is misspelled. It can be any value
#' between 0 and 1. The higher the value, the more suggestions are returned.
#' For more details, see \code{\link[base:agrep]{agrep}}. Default = 0.1.
#' @param include_subspecies (logical) whether to include subspecies. Default = FALSE
#' @param include_variety (logical) whether to include varieties. Default = FALSE
#' @param kingdom (character) the kingdom to which the species belong. It can
#' be "Plantae" or "Fungi". Default = "Plantae".
#' @param parallel (logical) whether to run in parallel. Setting this to `TRUE`
#' is recommended for improved performance when working with 100 or more species.
#' @param ncores (numeric) number of cores to use for parallel processing.
#' Default is 1. This is only applicable if `parallel = TRUE`.
#' @param progress_bar (logical) whether to display a progress bar during processing.
#' Default is FALSE
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
#' @usage check_names(data, species, max_distance = 0.1,
#'                    include_subspecies= FALSE, include_variety = FALSE,
#'                    kingdom = "Plantae", parallel = FALSE, ncores = 1,
#'                    progress_bar = FALSE)
#' @export
#' @references
#' Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#' @examples
#' data("bf_data", package = "florabr")
#' spp <- c("Butia cattarinensis", "Araucaria angustifolia")
#' check_names(data = bf_data, species = spp)

check_names <- function(data, species, max_distance = 0.1,
                        include_subspecies = FALSE,
                        include_variety = FALSE,
                        kingdom = "Plantae",
                        parallel = FALSE,
                        ncores = 1,
                        progress_bar = FALSE){
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

  #Taxon ranks
  if(include_subspecies){
    include_subspecies <-  "Subspecies"
  } else {
    include_subspecies <- NULL
  }
  if(include_variety){
    include_variety <-  "Variety"
  } else {
    include_variety <- NULL
  }

  txr <- c("Species", include_subspecies, include_variety)

  #Subset taxon ranks..
  data <- data[data$kingdom == kingdom & data$taxonRank %in% txr,]

  #Match names
  all_sp <- unique(data$species)

  spp <- match_names(species = species, species_to_match = all_sp,
                      max_distance = max_distance,
                      parallel = parallel, ncores = ncores,
                      progress_bar = progress_bar)

  #Create columns
  spp$Spelling <- ifelse(is.na(spp$Distance), "Not_found",
                         ifelse(spp$Distance > 0, "Probably_incorrect",
                                ifelse(spp$Distance == 0, "Correct", NA)))
  #Get information about Family, taxonomic and nomenclatural status
  d_info <- unique(data[data$species %in% spp$Suggested_name,
                 c("species", "taxonomicStatus", "nomenclaturalStatus",
                   "acceptedName", "family")])


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

#' @rdname check_names
#' @param species_to_match (character) a vector of species names to match against
#' the `species` parameter.
#' @importFrom parallel makeCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom utils txtProgressBar setTxtProgressBar adist
#' @importFrom foreach foreach `%dopar%`
#' @importFrom data.table rbindlist
#' @export
match_names <- function(species, species_to_match, max_distance = 0.1,
                        parallel = FALSE, ncores = 1, progress_bar = FALSE){
  if (missing(species_to_match)) {
    stop("Argument data is not defined")
  }
  if (missing(species)) {
    stop("Argument species is not defined")
  }
  if (!inherits(species_to_match, "character")) {
    stop(paste0("Argument species_to_match must be a character, not ",
                class(species_to_match)))
  }
  if (!is.character(species)) {
    stop(paste0("Argument species must be a character, not ",
                class(species)))
  }
  if (!is.numeric(max_distance)) {
    stop(paste0("Argument max_distance must be numeric, not ",
                class(max_distance)))
  }
  if (max_distance < 0 | max_distance > 1) {
    stop(paste0("Argument max_distance must be a numeric value between 0 and 1"))
  }

  sp_in <- intersect(species, species_to_match)
  if(length(sp_in) > 0){
    sp_in_df <- data.frame(input_name = sp_in, Suggested_name = sp_in,
                           Distance = 0)}

  sp_out <- setdiff(species,  species_to_match)

  if(length(sp_out) > 0){
  #Set parallel and progress bar
  if (progress_bar) {
    pb <- utils::txtProgressBar(min = 0, max = length(sp_out), style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)}

    if (parallel) {
      cl <- parallel::makeCluster(ncores)
      doSNOW::registerDoSNOW(cl)
      if (progress_bar){
        opts <- list(progress = progress)} else {opts <- opts}
    sp_l <- foreach::foreach(x = 1:length(sp_out), .options.snow = opts) %dopar% {
      sp_agrep <- agrep(sp_out[x], species_to_match, value = TRUE,
                        max.distance = max_distance)
      if (length(sp_agrep) > 0) {
        d <- as.numeric(adist(sp_out[x], sp_agrep))
      } else {
        sp_agrep <- NA
        d <- NA
      }
      sp_df <- data.frame(input_name = sp_out[x],
                          Suggested_name = sp_agrep,
                          Distance = d)
    } #End of dopar
    } else { #End of parallel
      sp_l <- vector("list", length = length(sp_out))
      for(x in 1:length(sp_out)){
        sp_agrep <- agrep(sp_out[x], species_to_match, value = TRUE,
                          max.distance = max_distance)
        if (length(sp_agrep) > 0) {
          d <- as.numeric(adist(sp_out[x], sp_agrep))
        } else {
          sp_agrep <- NA
          d <- NA
        }
        sp_l[[x]]<- data.frame(input_name = sp_out[x],
                            Suggested_name = sp_agrep,
                            Distance = d)
        # Sets the progress bar to the current state
        if(progress_bar){
          setTxtProgressBar(pb, x) }
      }
    } #End of else
  } #End of lenght sp_out

  #Merge results
  sp_l <- as.data.frame(rbindlist(sp_l))

  if(length(sp_in) > 0 & length(sp_out) > 0){
    sp_l <- rbind(sp_in_df, sp_l)}

  if(length(sp_in) > 0 & length(sp_out) == 0){
    sp_l <- sp_in_df}

  #Close parallel
  if(parallel)
    parallel::stopCluster(cl)

  return(sp_l)
}
