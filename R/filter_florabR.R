#' Identify records outside natural ranges according to Brazilian Flora 2020
#'
#' @description This function removes or flags records outside of the species'
#' natural ranges according to information provided by the Brazilian Flora 2020
#' database.
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function.
#' @param occ (data.frame) a data.frame with the records of the species.
#' @param Species (character) column name in occ with species names.
#' Default = "species"
#' @param Long (character) column name in occ with longitude data. Default = "x"
#' @param Lat (character) column name in occ with latitude data. Default = "y"
#' @param by_State (logical) filter records by state? Default = TRUE
#' @param buffer_State (numeric) buffer (in km) around the polygons of the
#' states of occurrence of the specie. Default = 20.
#' @param by_Biome (logical) filter records by Biome? Default = TRUE
#' @param buffer_Biome (numeric) buffer (in km) around the polygons of the
#' biomes of occurrence of the specie. Default = 20.
#' @param by_Endemism (logical) filter records by endemism? Default = TRUE
#' @param Buffer_Brazil (numeric) buffer (in km) around the polygons of the
#' Brazil. Default = 20.
#' @param State_vect (SpatVector) a SpatVector of the Brazilian states. By
#' default, it uses the SpatVector provided by geobr::read_state(). It can be
#' another Spatvector, but the structure must be identical to
#' geobr::read_state().
#' @param state_column (character) name of the column in State_vect containing
#' state abbreviations. Only use if Biome_vect is not null.
#' @param Biome_vect (SpatVector) a SpatVector of the Brazilian biomes. By
#' default, it uses the SpatVector provided by geobr::read_biomes(). It can be
#' another SpatVector, but the structure must be identical to
#' geobr::read_biomes() with biome names in English.
#' @param biome_column (character) name of the column in Biome_vect containing
#' names of brazilian biomes (in English: "Amazon", "Atlantic_Forest",
#' "Caatinga", "Cerrado", "Pampa" and "Pantanal". Only use if Biome_vect is not
#' null.
#' @param BR_vect (SpatVector) a SpatVector of Brazil. By default, it uses the
#' SpatVector provided by geobr::read_state() after being aggregated/dissolved,
#' @param value (character) Defines output values. See Value section.
#' Default = "flag&clean".
#' @param keep_columns (logical) if TRUE, keep all the original columns of the
#' input occ. If False, keep only the columns Species, Long and Lat.
#' Default = TRUE
#' @param verbose (logical) Whether to display species being filtered during
#' function execution. Set to TRUE to enable display, or FALSE to run silently.
#' Default = TRUE.
#' @details
#' If by_State = TRUE and/or by_Biome = TRUE, the function takes polygons
#' representing the states and/or Biomes with confirmed occurrences of the
#' specie, draws a buffer around the polygons, and tests if the records of the
#' species fall inside it.
#' If by_Endemism = TRUE, the function checks if the species is endemic to
#' Brazil. If it is endemic, the function tests if the records of the specie
#' fall inside a polygon representing the boundaries of Brazil (with a buffer).
#'
#'
#' @return Depending on the 'value' argument. If value = "flag", it returns the
#' same data.frame provided in data with additional columns indicating if the
#' record falls inside the natural range of the specie (TRUE) or outside
#' (FALSE).
#' If value = "clean", it returns a data.frame with only the records that passes
#' all the tests (TRUE for all the filters). If value = "flag&clean" (Default),
#' it returns a list with two data.frames: one with the flagged records and one
#' with the cleaned records.
#' @usage filter_florabr(data, occ, Species = "species", Long = "x", Lat = "y",
#'                       by_State = TRUE, buffer_State = 20, by_Biome = TRUE,
#'                       buffer_Biome = 20, by_Endemism = TRUE,
#'                       Buffer_Brazil = 20, State_vect = NULL,
#'                       state_column = NULL, Biome_vect = NULL,
#'                       biome_column = NULL, BR_vect = NULL,
#'                       value = "flag&clean", keep_columns = TRUE,
#'                       verbose = TRUE)
#' @export
#'
#' @importFrom terra aggregate subset buffer unwrap mask as.data.frame vect
#' @importFrom data.table rbindlist
#' @importFrom stats na.omit
#' @importFrom stats na.omit

#'
#' @references
#' Brazilian Flora 2020. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#' @examples
#' data("bf_data") #Load Brazilian Flora data
#' data("occurrences") #Load occurrences
#' pts <- subset(occurrences, species == "Myrcia hatschbachii")
#' fd <- filter_florabr(data = bf_data, occ = pts,
#'                     by_State = TRUE, buffer_State = 20,
#'                     by_Biome = TRUE, buffer_Biome = 20,
#'                     by_Endemism = TRUE, Buffer_Brazil = 20,
#'                     State_vect = NULL,
#'                     Biome_vect = NULL, BR_vect = NULL,
#'                     value = "flag&clean", keep_columns = TRUE,
#'                     verbose = FALSE)

filter_florabr <- function(data,
                          occ,
                          Species = "species", Long = "x", Lat = "y",
                          by_State = TRUE, buffer_State = 20,
                          by_Biome = TRUE, buffer_Biome = 20,
                          by_Endemism = TRUE, Buffer_Brazil = 20,
                          State_vect = NULL, state_column = NULL,
                          Biome_vect = NULL, biome_column = NULL,
                          BR_vect = NULL,
                          value = "flag&clean", keep_columns = TRUE,
                          verbose = TRUE) {
  if (missing(data)) {
    stop("Argument data is not defined")
  }

  if (missing(occ)) {
    stop("Argument occ is not defined")
  }
  #Check classes
  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ", class(data)))
  }

  if (!inherits(occ, "data.frame")) {
    stop(paste0("Argument occ must be a data.frame, not ", class(occ)))
  }

  if (!is.character(Species)) {
    stop(paste0("Argument Species must be a character, not ", class(Species)))
  }

  if (!is.character(Long)) {
    stop(paste0("Argument Long must be a character, not ", class(Long)))
  }

  if (!is.character(Lat)) {
    stop(paste0("Argument Lat must be a character, not ", class(Lat)))
  }

  if (!is.logical(by_State)) {
    stop(paste0("Argument by_State must be logical, not ", class(by_State)))
  }

  if (!is.logical(by_Biome)) {
    stop(paste0("Argument by_Biome must be logical, not ", class(by_Biome)))
  }

  if (!is.logical(by_Endemism)) {
    stop(paste0("Argument by_Endemism must be logical, not ",
                class(by_Endemism)))
  }

  if (!is.numeric(buffer_State)) {
    stop(paste0("Argument buffer_State must be numeric, not ",
                class(buffer_State)))
  }

  if (!is.numeric(buffer_Biome)) {
    stop(paste0("Argument buffer_Biome must be numeric, not ",
                class(buffer_Biome)))
  }

  if (!is.null(State_vect) && !inherits(State_vect, "SpatVector")) {
    stop(paste0("Argument State_vect must be a SpatVector, not ",
                class(State_vect)))
  }

  if (!is.null(Biome_vect) && !inherits(Biome_vect, "SpatVector")) {
    stop(paste0("Argument Biome_vect must be a SpatVector, not ",
                class(Biome_vect)))
  }

  if (!is.null(BR_vect) && !inherits(BR_vect, "SpatVector")) {
    stop(paste0("Argument BR_vect must be a SpatVector, not ", class(BR_vect)))
  }

  if (!is.null(State_vect) && !is.character(state_column)) {
    stop(paste0("Argument state_column must be a character, not ",
                class(state_column)))
  }

  if (!is.null(Biome_vect) && !is.character(biome_column)) {
    stop(paste0("Argument biome_column must be a character, not ",
                class(biome_column)))
  }

  allowed_values <- c("flag&clean", "flag", "clean")
  if (!(value %in% allowed_values)) {
    stop("Argument value must be 'flag', 'clean' or 'flag&clean'")
  }

  if (!is.logical(keep_columns)) {
    stop(paste0("Argument keep_columns must be logical, not ",
                class(keep_columns)))
  }

  if (!is.logical(verbose)) {
    stop(paste0("Argument verbose must be logical, not ", class(verbose)))
  }

  #Check colnames in data
  if(!all(c("species", "States", "Biome", "Endemism") %in%
        colnames(data))) {
    stop("Important columns are missing in data. Check if data is an object
         created by 'load_florabr()")
  }

  if(!all(c(Species, Long, Lat) %in%
          colnames(occ))) {
    stop("Important columns are missing in occurrence data. Check if correct
         column names were set in Species, Long and Lat")
  }

  #Load data
  d <- data[,c("species", "States", "Biome", "Endemism")]

  #Create columns with ID to join data in the end
  occ$id_f <- seq_len(nrow(occ))

  #Get species info
  occ_info <- occ[, c(Species, Long, Lat, "id_f")]
  colnames(occ_info) <- c("species", "x", "y", "id_f")

  #Check if all species are in Brazilin Flora data
  spp <- unique(occ_info$species)
  spp_out <- setdiff(spp, unique(data$species))
  if(length(spp_out) > 0) {
    stop(paste(length(spp_out), "species are not in the data. Check the species
               names using the check_names() function or remove the species from
               data.frame"))
  }

  d_info <- subset(d, d$species %in% unique(occ_info$species))
  d_info[d_info == ""] <- NA
  #Get only one line by species, merging information of same species
  sp_info <- lapply(seq_along(spp), function(i) {
    sp <- subset(d_info, d_info$species == spp[i])
    sp$States <- paste0(na.omit(unique(sp$States)),
                            collapse = ";")
    sp$Biome <- paste0(na.omit(unique(sp$Biome)),
                                       collapse = ";")
    sp$Endemism <- paste0(na.omit(unique(sp$Endemism)),
                          collapse = ";")
    return(sp)
  })
  sp_info <- unique(data.table::rbindlist(sp_info))


  #Merge data
  occ_info <- merge(occ_info, sp_info, by = "species")
  #Convert to SpatVector (faster!)
  occ_info <- terra::vect(occ_info, geom = c("x", "y"))

  #Load vectors
  brazil <- terra::unwrap(florabr::brazil)
  if(by_State == TRUE) {
  states <- terra::unwrap(florabr::states)
  }
  if(by_Biome == TRUE) {
  biomes <- terra::unwrap(florabr::biomes)
  }

  #Check personal vectors (if provided)
    #States
  if(!is.null(State_vect)){
    names(State_vect)[which(names(State_vect) ==
                              state_column)] <- "abbrev_state"
    check_matches <- setdiff(states$abbrev_state, State_vect$abbrev_state)
    if(length(check_matches) > 0) {
      stop(paste0("Invalid States in ", state_column,
      "\nCheck the structure of the Spatvector provided in State_vect"))
    } else {
      states <- State_vect
    }}
    #Biomes
  if(!is.null(Biome_vect)){
    names(Biome_vect)[which(names(Biome_vect) == biome_column)] <- "name_biome"
    check_matches <- setdiff(biomes$name_biome, Biome_vect$name_biome)
    if(length(check_matches) > 0) {
      stop(paste0("Invalid Biomes in ", biome_column,
                  "\nCheck the structure of the Spatvector provided in
                  Biome_vect"))
    } else {
      biomes <- Biome_vect
    }}

  #Filter by state
  if(by_State == FALSE) {
    occ_state <- occ_info
    }

  if(by_State == TRUE) {
   l_state <- lapply(seq_along(spp), function(i){
     if(verbose) {
       message("Filtering", spp[i], "by state\n") }
     occ_i <- subset(occ_info, occ_info$species == spp[i])
     sp_i_state <- unique(gsub(";", "|", occ_i$States[1]))

     if(sp_i_state == "" | is.na(sp_i_state)) {
       if(verbose) {
       message(spp[i], "lacks info about State - Filter not applicable\n") }
       states_ext <- occ_i
       states_ext$Inside_State <- "No info"
     } else {
     states_v <- terra::aggregate(
       terra::subset(states, grepl(sp_i_state, states$abbrev_state)))

     #Buffer
     states_v <- terra::buffer(states_v, width =buffer_State*1000)

     #Get only records that fall inside Brazil
     occ_br <- occ_i[brazil]
     occ_out <- terra::mask(occ_i, brazil, inverse = TRUE)
     occ_out$Inside_State <- NA

    #Get records that falls inside states
     occ_in_states <- occ_br[states_v]
     occ_in_states$Inside_State <- TRUE
     occ_out_states <- terra::mask(occ_br, states_v, inverse = TRUE)
     occ_out_states$Inside_State <- FALSE

    #Merge data
     states_final <- rbind(occ_in_states, occ_out, occ_out_states)
     }
     return(states_final)
      })
   occ_state <- do.call("rbind", l_state)
  }

  #Filter by biome
  if(by_Biome == FALSE) {
    occ_biome <- occ_state
    occ_biome$Inside_Biome <- NA}

  if(by_Biome == TRUE) {
    l_biome <- lapply(seq_along(spp), function(i){
      if(verbose) {
      message("Filtering", spp[i], "by biome\n") }
      occ_i <- terra::subset(occ_state, occ_state$species == spp[i])
      sp_i_biome <- unique(gsub(";", "|", occ_i$Biome[1]))


      if(sp_i_biome == "" | is.na(sp_i_biome)) {
        if(verbose) {
        message(spp[i], "lacks info about Biome - Filter not applicable\n") }
        biomes_ext <- occ_i
        biomes_ext$Inside_Biome <- "No info"
      } else {
        biomes_v <- terra::aggregate(
          terra::subset(biomes, grepl(sp_i_biome, biomes$name_biome)))

        #Buffer
        biomes_v <- terra::buffer(biomes_v, width =buffer_Biome*1000)

        #Get only records that fall inside Brazil
        occ_br <- occ_i[brazil]
        occ_out <- terra::mask(occ_i, brazil, inverse = TRUE)
        occ_out$Inside_Biome <- NA

        #Get records that falls inside biomes
        occ_in_biomes <- occ_br[biomes_v]
        occ_in_biomes$Inside_Biome <- TRUE
        occ_out_biomes <- terra::mask(occ_br, biomes_v, inverse = TRUE)
        occ_out_biomes$Inside_Biome <- FALSE

        #Merge data
        biomes_final <- rbind(occ_in_biomes, occ_out, occ_out_biomes)
      }
      return(biomes_final)
    })
    occ_biome <- do.call("rbind", l_biome)
  }


  #Filter by Endemism
  if(by_Endemism == FALSE) {
    occ_flag <- terra::as.data.frame(occ_biome)
    occ_flag$Inside_BR <- NA}

  if(by_Endemism == TRUE) {
    l_end <- lapply(seq_along(spp), function(i){
      occ_i <- terra::subset(occ_biome, occ_biome$species == spp[i])
      sp_i_end <- unique(gsub(";", "|", occ_i$Endemism[1]))

      if(sp_i_end != "Endemic") {
        if(verbose) {
        message(spp[i], "is non-endemic - Filter not applicable\n") }
        occ_BR <- terra::as.data.frame(occ_i)
        occ_BR$Inside_BR <- "Non-endemic"
      } else {
        if(verbose) {
        message("Filtering", spp[i], "by endemism\n") }
        #Buffer
      BR_v <- terra::buffer(brazil, width = Buffer_Brazil*1000)

      #Split points inside and outside Brazil
      occ_in_BR <- occ_i[brazil]
      occ_in_BR$Inside_BR <- TRUE
      occ_out_BR <- terra::mask(occ_i, brazil, inverse = TRUE)
      occ_out_BR$Inside_BR <- FALSE
      #Merge data
      occ_BR <- rbind(occ_in_BR, occ_out_BR)
      occ_BR <- terra::as.data.frame(occ_BR)
      }
      return(occ_BR)
    })
    occ_flag <- data.frame(data.table::rbindlist(l_end))
  }

  #Merge original data
  if(isTRUE(keep_columns)) {
    occ_flag <- merge(occ_flag, occ, by = c("species", "id_f"))
    occ_flag$id_f <- NULL
    occ_flag <- occ_flag[, c(Species, Long, Lat,
                             colnames(occ_flag)[!(colnames(occ_flag) %in%
                            c(Species, Long, Lat))])]

    colnames(occ_flag)[colnames(occ_flag) %in%
                         c(Species, Long, Lat)] <- c(Species, Long, Lat)
  }

  if(isFALSE(keep_columns)) {
    occ_flag <- merge(occ_flag, occ[, c(Species, Lat, Long, "id_f")],
                      by = c("species", "id_f"))
    occ_flag$id_f <- NULL
    occ_flag <- occ_flag[,
                         c(Species, Long, Lat, names(occ_flag)[!(names(occ_flag)
                                                                 %in%
                                                      c(Species, Long, Lat))])]
    colnames(occ_flag)[colnames(occ_flag) %in%
                         c(Species, Long, Lat)] <- c(Species, Long, Lat)
  }


  #Remove columns of filters not applicable
  if(by_State == FALSE) {occ_flag$Inside_State <- NULL}
  if(by_Biome == FALSE) {occ_flag$Inside_Biome <- NULL}
  if(by_Endemism == FALSE) {occ_flag$Inside_BR <- NULL}

  col_check <- intersect(c("Inside_State", "Inside_Biome", "Inside_BR"),
                         names(occ_flag))

  if(length(col_check) == 1) {
    occ_flag$Filters_ok <- ifelse(occ_flag[,col_check] == TRUE |
                                    is.na(occ_flag[,col_check]), TRUE, FALSE)
  } else {
  occ_flag$Filters_ok <- ifelse(rowSums(!is.na(occ_flag[, col_check]) &
                                 occ_flag[, col_check] == FALSE) > 0, FALSE,
                                 TRUE) }
  #Filter problematic points
  occ_clean <- subset(occ_flag, occ_flag$Filters_ok == TRUE)
  occ_clean <- subset(occ_clean, select = setdiff(colnames(occ_clean),
                                               c(col_check, "Filters_ok")))

  #Get final result
  if(value == "flag&clean") {
    res_final <- list(occ_flag, occ_clean)
    names(res_final) <- c("flagged", "cleaned")
    if(verbose) {
    message("Returning list with flagged and cleaned occurrences\n")}
  }

  if(value == "flag") {
    res_final <- occ_flag
    if(verbose) {
    message("Returning dataframe with flagged occurrences\n")}
  }

  if(value == "clean") {
    res_final <- occ_clean
    if(verbose) {
    message("Returning dataframe with cleaned occurrences")}
  }

  return(res_final)
  } #End of function






