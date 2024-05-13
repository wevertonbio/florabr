#' Identify records outside natural ranges according to Flora e Funga do Brasil
#'
#' @description This function removes or flags records outside of the species'
#' natural ranges according to information provided by the Flora e Funga do
#' Brasil database.
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function.
#' @param occ (data.frame) a data.frame with the records of the species.
#' @param species (character) column name in occ with species names.
#' Default = "species"
#' @param long (character) column name in occ with longitude data. Default = "x"
#' @param lat (character) column name in occ with latitude data. Default = "y"
#' @param by_state (logical) filter records by state? Default = TRUE
#' @param buffer_state (numeric) buffer (in km) around the polygons of the
#' states of occurrence of the specie. Default = 20.
#' @param by_biome (logical) filter records by biome? Default = TRUE
#' @param buffer_biome (numeric) buffer (in km) around the polygons of the
#' biomes of occurrence of the specie. Default = 20.
#' @param by_endemism (logical) filter records by endemism? Default = TRUE
#' @param buffer_brazil (numeric) buffer (in km) around the polygons of the
#' brazil. Default = 20.
#' @param state_vect (SpatVector) a SpatVector of the Brazilian states. By
#' default, it uses the SpatVector provided by geobr::read_state(). It can be
#' another Spatvector, but the structure must be identical to
#' geobr::read_state().
#' @param state_column (character) name of the column in state_vect containing
#' state abbreviations. Only use if biome_vect is not null.
#' @param biome_vect (SpatVector) a SpatVector of the Brazilian biomes. By
#' default, it uses the SpatVector provided by geobr::read_biomes(). It can be
#' another SpatVector, but the structure must be identical to
#' geobr::read_biomes() with biome names in English.
#' @param biome_column (character) name of the column in biome_vect containing
#' names of brazilian biomes (in English: "Amazon", "Atlantic_Forest",
#' "Caatinga", "Cerrado", "Pampa" and "Pantanal". Only use if biome_vect is not
#' null.
#' @param br_vect (SpatVector) a SpatVector of brazil. By default, it uses the
#' SpatVector provided by geobr::read_state() after being aggregated/dissolved,
#' @param value (character) Defines output values. See Value section.
#' Default = "flag&clean".
#' @param keep_columns (logical) if TRUE, keep all the original columns of the
#' input occ. If False, keep only the columns species, long and lat.
#' Default = TRUE
#' @param verbose (logical) Whether to display species being filtered during
#' function execution. Set to TRUE to enable display, or FALSE to run silently.
#' Default = TRUE.
#' @details
#' If by_state = TRUE and/or by_biome = TRUE, the function takes polygons
#' representing the states and/or biomes with confirmed occurrences of the
#' specie, draws a buffer around the polygons, and tests if the records of the
#' species fall inside it.
#' If by_endemism = TRUE, the function checks if the species is endemic to
#' brazil. If it is endemic, the function tests if the records of the specie
#' fall inside a polygon representing the boundaries of brazil (with a buffer).
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
#' @usage filter_florabr(data, occ, species = "species", long = "x", lat = "y",
#'                       by_state = TRUE, buffer_state = 20, by_biome = TRUE,
#'                       buffer_biome = 20, by_endemism = TRUE,
#'                       buffer_brazil = 20, state_vect = NULL,
#'                       state_column = NULL, biome_vect = NULL,
#'                       biome_column = NULL, br_vect = NULL,
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
#' Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#' @examples
#' data("bf_data") #Load Flora e Funga do Brasil data
#' data("occurrences") #Load occurrences
#' pts <- subset(occurrences, species == "Myrcia hatschbachii")
#' fd <- filter_florabr(data = bf_data, occ = pts,
#'                     by_state = TRUE, buffer_state = 20,
#'                     by_biome = TRUE, buffer_biome = 20,
#'                     by_endemism = TRUE, buffer_brazil = 20,
#'                     state_vect = NULL,
#'                     biome_vect = NULL, br_vect = NULL,
#'                     value = "flag&clean", keep_columns = TRUE,
#'                     verbose = FALSE)

filter_florabr <- function(data,
                          occ,
                          species = "species", long = "x", lat = "y",
                          by_state = TRUE, buffer_state = 20,
                          by_biome = TRUE, buffer_biome = 20,
                          by_endemism = TRUE, buffer_brazil = 20,
                          state_vect = NULL, state_column = NULL,
                          biome_vect = NULL, biome_column = NULL,
                          br_vect = NULL,
                          value = "flag&clean", keep_columns = TRUE,
                          verbose = TRUE) {
  if (missing(data)) {
    stop("Argument data is not defined")
  }
  if (missing(occ)) {
    stop("Argument occ is not defined")
  }
  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ",
                class(data)))
  }
  if (!inherits(occ, "data.frame")) {
    stop(paste0("Argument occ must be a data.frame, not ",
                class(occ)))
  }
  if (!is.character(species)) {
    stop(paste0("Argument species must be a character, not ",
                class(species)))
  }
  if (!is.character(long)) {
    stop(paste0("Argument long must be a character, not ",
                class(long)))
  }
  if (!is.character(lat)) {
    stop(paste0("Argument lat must be a character, not ",
                class(lat)))
  }
  if (!is.logical(by_state)) {
    stop(paste0("Argument by_state must be logical, not ",
                class(by_state)))
  }
  if (!is.logical(by_biome)) {
    stop(paste0("Argument by_biome must be logical, not ",
                class(by_biome)))
  }
  if (!is.logical(by_endemism)) {
    stop(paste0("Argument by_endemism must be logical, not ",
                class(by_endemism)))
  }
  if (!is.numeric(buffer_state)) {
    stop(paste0("Argument buffer_state must be numeric, not ",
                class(buffer_state)))
  }
  if (!is.numeric(buffer_biome)) {
    stop(paste0("Argument buffer_biome must be numeric, not ",
                class(buffer_biome)))
  }
  if (!is.null(state_vect) && !inherits(state_vect, "SpatVector")) {
    stop(paste0("Argument state_vect must be a SpatVector, not ",
                class(state_vect)))
  }
  if (!is.null(biome_vect) && !inherits(biome_vect, "SpatVector")) {
    stop(paste0("Argument biome_vect must be a SpatVector, not ",
                class(biome_vect)))
  }
  if (!is.null(br_vect) && !inherits(br_vect, "SpatVector")) {
    stop(paste0("Argument br_vect must be a SpatVector, not ",
                class(br_vect)))
  }
  if (!is.null(state_vect) && !is.character(state_column)) {
    stop(paste0("Argument state_column must be a character, not ",
                class(state_column)))
  }
  if (!is.null(biome_vect) && !is.character(biome_column)) {
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
    stop(paste0("Argument verbose must be logical, not ",
                class(verbose)))
  }

  #Convert colnames to lower case
  original_colnames <- colnames(data)
  colnames(data) <- tolower(colnames(data))

  if (!all(c("species", "states", "biome", "endemism") %in%
           colnames(data))) {
    stop("Important columns are missing in data. Check if data is an object\n         created by 'load_florabr()")
  }
  if (!all(c(species, long, lat) %in% colnames(occ))) {
    stop("Important columns are missing in occurrence data. Check if correct\n         column names were set in species, long and lat")
  }
  d <- data[, c("species", "states", "biome", "endemism")]
  occ$id_f <- seq_len(nrow(occ))
  occ_info <- occ[, c(species, long, lat, "id_f")]
  colnames(occ_info) <- c("species", "x", "y", "id_f")
  spp <- unique(occ_info$species)
  spp_out <- setdiff(spp, unique(data$species))
  if (length(spp_out) > 0) {
    stop(paste(length(spp_out), "species are not in the data. Check the species\n               names using the check_names() function or remove the species from\n               data.frame"))
  }
  d_info <- subset(d, d$species %in% unique(occ_info$species))
  d_info[d_info == ""] <- NA
  sp_info <- lapply(seq_along(spp), function(i) {
    sp <- subset(d_info, d_info$species == spp[i])
    sp$states <- paste0(na.omit(unique(sp$states)), collapse = ";")
    sp$biome <- paste0(na.omit(unique(sp$biome)), collapse = ";")
    sp$endemism <- paste0(na.omit(unique(sp$endemism)),
                          collapse = ";")
    return(sp)
  })
  sp_info <- unique(data.table::rbindlist(sp_info))
  occ_info <- merge(occ_info, sp_info, by = "species")
  occ_info <- terra::vect(occ_info, geom = c("x", "y"))
  brazil <- terra::unwrap(florabr::brazil)
  if (by_state == TRUE) {
    states <- terra::unwrap(florabr::states)
  }
  if (by_biome == TRUE) {
    biomes <- terra::unwrap(florabr::biomes)
  }
  if (!is.null(state_vect)) {
    names(state_vect)[which(names(state_vect) == state_column)] <- "abbrev_state"
    check_matches <- setdiff(states$abbrev_state, state_vect$abbrev_state)
    if (length(check_matches) > 0) {
      stop(paste0("Invalid states in ", state_column,
                  "\nCheck the structure of the Spatvector provided in state_vect"))
    }
    else {
      states <- state_vect
    }
  }
  if (!is.null(biome_vect)) {
    names(biome_vect)[which(names(biome_vect) == biome_column)] <- "name_biome"
    check_matches <- setdiff(biomes$name_biome, biome_vect$name_biome)
    if (length(check_matches) > 0) {
      stop(paste0("Invalid biomes in ", biome_column,
                  "\nCheck the structure of the Spatvector provided in\n                  biome_vect"))
    }
    else {
      biomes <- biome_vect
    }
  }
  if (by_state == FALSE) {
    occ_state <- occ_info
  }
  if (by_state == TRUE) {
    l_state <- lapply(seq_along(spp), function(i) {
      if (verbose) {
        message("Filtering", spp[i], "by state\n")
      }
      occ_i <- subset(occ_info, occ_info$species == spp[i])
      sp_i_state <- unique(gsub(";", "|", occ_i$states[1]))
      if (sp_i_state == "" | is.na(sp_i_state)) {
        if (verbose) {
          message(spp[i], "lacks info about state - Filter not applicable\n")
        }
        states_final <- occ_i
        states_final$inside_state <- "No info"
      }
      else {
        states_v <- terra::aggregate(terra::subset(states,
                                                   grepl(sp_i_state, states$abbrev_state)))
        states_v <- terra::buffer(states_v, width = buffer_state *
                                    1000)
        occ_br <- occ_i[brazil]
        occ_out <- terra::mask(occ_i, brazil, inverse = TRUE)
        occ_out$inside_state <- NA
        occ_in_states <- occ_br[states_v]
        occ_in_states$inside_state <- TRUE
        occ_out_states <- terra::mask(occ_br, states_v,
                                      inverse = TRUE)
        occ_out_states$inside_state <- FALSE
        states_final <- rbind(occ_in_states, occ_out,
                              occ_out_states)
      }
      return(states_final)
    })
    occ_state <- do.call("rbind", l_state)
  }
  if (by_biome == FALSE) {
    occ_biome <- occ_state
    occ_biome$inside_biome <- NA
  }
  if (by_biome == TRUE) {
    l_biome <- lapply(seq_along(spp), function(i) {
      if (verbose) {
        message("Filtering", spp[i], "by biome\n")
      }
      occ_i <- terra::subset(occ_state, occ_state$species ==
                               spp[i])
      sp_i_biome <- unique(gsub(";", "|", occ_i$biome[1]))
      if (sp_i_biome == "" | is.na(sp_i_biome)) {
        if (verbose) {
          message(spp[i], "lacks info about biome - Filter not applicable\n")
        }
        biomes_final <- occ_i
        biomes_final$inside_biome <- "No info"
      }
      else {
        biomes_v <- terra::aggregate(terra::subset(biomes,
                                                   grepl(sp_i_biome, biomes$name_biome)))
        biomes_v <- terra::buffer(biomes_v, width = buffer_biome *
                                    1000)
        occ_br <- occ_i[brazil]
        occ_out <- terra::mask(occ_i, brazil, inverse = TRUE)
        occ_out$inside_biome <- NA
        occ_in_biomes <- occ_br[biomes_v]
        occ_in_biomes$inside_biome <- TRUE
        occ_out_biomes <- terra::mask(occ_br, biomes_v,
                                      inverse = TRUE)
        occ_out_biomes$inside_biome <- FALSE
        biomes_final <- rbind(occ_in_biomes, occ_out,
                              occ_out_biomes)
      }
      return(biomes_final)
    })
    occ_biome <- do.call("rbind", l_biome)
  }
  if (by_endemism == FALSE) {
    occ_flag <- terra::as.data.frame(occ_biome)
    occ_flag$inside_br <- NA
  }
  if (by_endemism == TRUE) {
    l_end <- lapply(seq_along(spp), function(i) {
      occ_i <- terra::subset(occ_biome, occ_biome$species ==
                               spp[i])
      sp_i_end <- unique(gsub(";", "|", occ_i$endemism[1]))
      if (sp_i_end != "Endemic") {
        if (verbose) {
          message(spp[i], "is non-endemic - Filter not applicable\n")
        }
        occ_BR <- terra::as.data.frame(occ_i)
        occ_BR$inside_br <- "Non-endemic"
      }
      else {
        if (verbose) {
          message("Filtering", spp[i], "by endemism\n")
        }
        br_v <- terra::buffer(brazil, width = buffer_brazil *
                                1000)
        occ_in_BR <- occ_i[brazil]
        occ_in_BR$inside_br <- TRUE
        occ_out_BR <- terra::mask(occ_i, brazil, inverse = TRUE)
        occ_out_BR$inside_br <- FALSE
        occ_BR <- rbind(occ_in_BR, occ_out_BR)
        occ_BR <- terra::as.data.frame(occ_BR)
      }
      return(occ_BR)
    })
    occ_flag <- data.frame(data.table::rbindlist(l_end))
  }
  if (isTRUE(keep_columns)) {
    occ_flag <- merge(occ_flag, occ, by = c("species", "id_f"))
    occ_flag$id_f <- NULL
    occ_flag <- occ_flag[, c(species, long, lat, colnames(occ_flag)[!(colnames(occ_flag) %in%
                                                                        c(species, long, lat))])]
    colnames(occ_flag)[colnames(occ_flag) %in% c(species,
                                                 long, lat)] <- c(species, long, lat)
  }
  if (isFALSE(keep_columns)) {
    occ_flag <- merge(occ_flag, occ[, c(species, lat, long,
                                        "id_f")], by = c("species", "id_f"))
    occ_flag$id_f <- NULL
    occ_flag <- occ_flag[, c(species, long, lat, names(occ_flag)[!(names(occ_flag) %in%
                                                                     c(species, long, lat))])]
    colnames(occ_flag)[colnames(occ_flag) %in% c(species,
                                                 long, lat)] <- c(species, long, lat)
  }
  if (by_state == FALSE) {
    occ_flag$inside_state <- NULL
  }
  if (by_biome == FALSE) {
    occ_flag$inside_biome <- NULL
  }
  if (by_endemism == FALSE) {
    occ_flag$inside_br <- NULL
  }
  col_check <- intersect(c("inside_state", "inside_biome",
                           "inside_br"), names(occ_flag))
  if (length(col_check) == 1) {
    occ_flag$filters_ok <- ifelse(occ_flag[, col_check] ==
                                    TRUE | is.na(occ_flag[, col_check]), TRUE, FALSE)
  }  else {
    occ_flag$filters_ok <- ifelse(rowSums(!is.na(occ_flag[,
                                                          col_check]) & occ_flag[, col_check] == FALSE) >
                                    0, FALSE, TRUE)
  }
  occ_clean <- subset(occ_flag, occ_flag$filters_ok == TRUE)
  occ_clean <- subset(occ_clean, select = setdiff(colnames(occ_clean),
                                                  c(col_check, "filters_ok")))
  if (value == "flag&clean") {
    res_final <- list(occ_flag, occ_clean)
    names(res_final) <- c("flagged", "cleaned")
    if (verbose) {
      message("Returning list with flagged and cleaned occurrences\n")
    }
  }
  if (value == "flag") {
    res_final <- occ_flag
    if (verbose) {
      message("Returning dataframe with flagged occurrences\n")
    }
  }
  if (value == "clean") {
    res_final <- occ_clean
    if (verbose) {
      message("Returning dataframe with cleaned occurrences")
    }
  }
  return(res_final)
}
