#' Get Spatial polygons (SpatVectors) of species based on its distribution
#' (states and biomes) according to Flora e Funga do Brasil
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function.
#' @param species (character) one or more species names (only genus and
#' specific epithet, eg. "Araucaria angustifolia")
#' @param state (logical) get SpatVector of states with occurrence of the
#' species? Default = TRUE
#' @param biome (logical) get SpatVector of biomes with occurrence of the
#' species? Default = TRUE
#' @param intersection (character) get a Spatvector representing the
#' intersection between states and biomes with occurrence of the specie?
#' To use intersection = TRUE, you must define state = TRUE and biome = TRUE".
#' Default = TRUE
#' @param state_vect (SpatVector) a SpatVector of the Brazilian states. By
#' default, it uses the SpatVector provided by geobr::read_state(). It can be
#' another Spatvector, but the structure must be identical to
#' geobr::read_state().
#' @param state_column (character) name of the column in state_vect containing
#' state abbreviations. Only use if biome_vect is not null.
#' @param biome_vect (SpatVector) a SpatVector of the Brazilian biomes. By
#' default, it uses the SpatVector provided by geobr::read_biomes(). It can be
#' another SpatVector, but the structure must be identical to
#' geobr::read_biomes().
#' @param biome_column (character) name of the column in biome_vect containing
#' names of brazilian biomes (in English: "Amazon", "Atlantic_Forest",
#' "Caatinga", "Cerrado", "Pampa" and "Pantanal". Only use if biome_vect is not
#'  null.
#' @param verbose (logical) Whether to display species being filtered during
#' function execution. Set to TRUE to enable display, or FALSE to run silently.
#' Default = TRUE.
#'
#' @return A list with SpatVectors of states and/or biomes and/or Intersections
#' for each specie.
#' @importFrom terra subset unwrap intersect mask
#' @importFrom data.table rbindlist
#' @export
#' @references
#' Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#' @examples
#' library(terra)
#' data("bf_data") #Load Flora e Funga do Brasil data
#' spp <- c("Araucaria angustifolia", "Adesmia paranensis") #Example species
#' #Get states, biomes and intersection states-biomes of species
#' spp_spt <- get_spat_occ(data = bf_data, species = spp, state = TRUE,
#'                        biome = TRUE, intersection = TRUE, state_vect = NULL,
#'                        biome_vect = NULL, verbose = TRUE)
#'
#'
#' #Plot states of occurrence of Araucaria angustifolia
#' plot(spp_spt[[1]]$states, main = names(spp_spt)[[1]])
#' #Plot biomes of occurrence of Araucaria angustifolia
#' plot(spp_spt[[2]]$biomes, main = names(spp_spt)[[2]])
#' #Plot intersection between states and biomes of occurrence of
#' #Araucaria angustifolia
#' plot(spp_spt[[1]]$states_biomes)
#'
get_spat_occ <- function(data, species, state = TRUE,
                         biome = TRUE,
                         intersection = TRUE,
                         state_vect = NULL, state_column = NULL,
                         biome_vect = NULL, biome_column = NULL,
                         verbose = TRUE) {
  if (missing(data)) {
    stop("Argument data is not defined")
  }

  if (missing(species)) {
    stop("Argument occ is not defined")
  }

  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ", class(data)))
  }

  if (!is.character(species)) {
    stop(paste0("Argument species must be a character, not ", class(species)))
  }

  if (!is.logical(state)) {
    stop(paste0("Argument state must be logical, not ", class(state)))
  }

  if (!is.logical(biome)) {
    stop(paste0("Argument biome must be logical, not ", class(biome)))
  }

  if (!is.logical(intersection)) {
    stop(paste0("Argument intersection must be logical, not ",
                class(intersection)))
  }

  if (!is.null(state_vect) && !inherits(state_vect, "SpatVector")) {
    stop(paste0("Argument state_vect must be NULL or a SpatVector, not ",
                class(state_vect)))
  }

  if (!is.null(biome_vect) && !inherits(biome_vect, "SpatVector")) {
    stop(paste0("Argument biome_vect must be NULL or a SpatVector, not ",
                class(biome_vect)))
  }

  if (!is.null(state_vect) && !is.character(state_column)) {
    stop(paste0("Argument state_column must be a character, not ",
                class(state_column)))
  }

  if (!is.null(biome_vect) && !is.character(biome_column)) {
    stop(paste0("Argument biome_column must be a character, not ",
                class(biome_column)))
  }

  if (!is.logical(verbose)) {
    stop(paste0("Argument verbose must be logical, not ", class(verbose)))
  }

  #Check colnames in data
  if(!all(c("species", "states", "biome") %in%
          colnames(data))) {
    stop("Important columns are missing in data. Check if data is an object
         created by 'load_florabr()")
  }

  #Check if there is at least one TRUE in states or biomes
  if(!state & !biome){
    stop("At least one of the parameters state or biome must be TRUE")
  }
  if(intersection & (!state | !biome)) {
    stop("To use intersection = TRUE, you must define state = TRUE and
         biome = TRUE")
  }

  #Load data
  d <- data[,c("species", "states", "biome")]

  #Check if all species are in Flora e Funga do Brasil data
  spp <- get_binomial(species_names = species)
  #Get binomial names of species
  spp_out <- setdiff(spp, unique(data$species))
  if(length(spp_out) > 0) {
    stop(paste(length(spp_out), "species are not in the data. Check the species
               names using the check_names() function"))
  }
  #Subset info
  d_info <- subset(d, d$species %in% spp)
  d_info[d_info == ""] <- NA

  #Get only one line by species, merging information of same species
  sp_info <- lapply(seq_along(spp), function(i) {
    sp <- subset(d_info, d_info$species == spp[i])
    sp$states <- paste0(na.omit(unique(sp$states)),
                        collapse = ";")
    sp$biome <- paste0(na.omit(unique(sp$biome)),
                       collapse = ";")
    return(sp)
  })
  sp_info <- unique(data.table::rbindlist(sp_info))

  #Load data
  if(state) {
  states <- terra::unwrap(florabr::states)
  }
  if(biome) {
    biomes <- terra::unwrap(florabr::biomes)
  }

  #Check personal vectors (if provided)
  #states
  if(!is.null(state_vect)){
    names(state_vect)[which(names(state_vect) == state_column)] <-"abbrev_state"
    check_matches <- setdiff(states$abbrev_state, state_vect$abbrev_state)
    if(length(check_matches) > 0) {
      stop(paste0("Invalid states in ", state_column,
                  "\nCheck the structure of the Spatvector provided in
                  state_vect"))
    } else {
      states <- state_vect
    }}
  #biomes
  if(!is.null(biome_vect)){
    names(biome_vect)[which(names(biome_vect) == biome_column)] <- "name_biome"
    check_matches <- setdiff(biomes$name_biome, biome_vect$name_biome)
    if(length(check_matches) > 0) {
      stop(paste0("Invalid biomes in ", biome_column,
                  "\nCheck the structure of the Spatvector provided in
                  biome_vect"))
    } else {
      biomes <- biome_vect
    }}

  #Get state
  l_occ <- lapply(seq_along(spp), function(i){
    occ_i <- subset(sp_info, sp_info$species == spp[i])

    if(!state) {states_v <- NULL}

    if(state) {
      if(verbose) {
        message("Getting states of ", spp[i], "\n") }

      sp_i_state <- unique(gsub(";", "|", occ_i$states[1]))

      if(sp_i_state == "" | is.na(sp_i_state)) {
        if(verbose) {
        message(spp[i], "lacks info about state - SpatialVector not
                  returned")}
        states_v <- "No_info"
      } else {
        states_v <- terra::subset(states, grepl(sp_i_state,
                                                    states$abbrev_state)) }
    }

    if(!biome) {biomes_v <- NULL}

    if(biome) {
      if(verbose) {
        message("Getting biomes of ", spp[i], "\n") }
      sp_i_biome<- unique(gsub(";", "|", occ_i$biome[1]))

      if(sp_i_biome == "" | is.na(sp_i_biome)) {
        if(verbose){
        message(spp[i], "lacks info about biome - SpatialVector not
                  returned")}
        biomes_v <- "No_info"
      } else {
        biomes_v <- terra::subset(biomes, grepl(sp_i_biome,
                                                    biomes$name_biome)) }
    }

    if(!intersection) {int_v <- NULL}

    if(intersection) {
      if(verbose) {
        message("Getting biomes of ", spp[i], "\n") }
      if((sp_i_biome == "" | is.na(sp_i_biome)) & verbose) {
        message(spp[i], "lacks info about states - Impossible to get
                  intersection with states")
      }
      if((sp_i_biome == "" | is.na(sp_i_biome)) & verbose) {
        message(spp[i], "lacks info about biomes - Impossible to get
                  intersection with biomes")
      }


    int_v <- terra::intersect(biomes_v, states_v) }

    #Save objects in a list
    final_list <- list(states_v, biomes_v, int_v)
    names(final_list) <- c("states", "biomes", "states_biomes")
    return(final_list)
  })
  names(l_occ) <- spp
  #Drop off null elements
  l_occ <- lapply(l_occ, function(x) x[lengths(x) > 0])
  return(l_occ)
} #End if function
