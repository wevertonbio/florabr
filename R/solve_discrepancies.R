#' Resolve discrepancies between species and subspecies/varieties information
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function.
#'
#' @return a data.frame with the discrepancies solved
#' @usage solve_discrepancies(data)
#' @details
#' In the original dataset, discrepancies may exist between species and
#' subspecies/varieties information. An example of a discrepancy is when a
#' species occurs only in one biome (e.g., Amazon), but a subspecies or variety
#' of the same species occurs in another biome (e.g., Cerrado). This function
#' rectifies such discrepancies by considering distribution (states, biomes,
#' and vegetation), life form, and habitat. For instance, if a subspecies is
#' recorded in a specific biome, it implies that the species also occurs in that
#' biome.
#'
#' @export
#'
#' @examples
#' data("bf_data") #Load Flora e Funga do Brasil data
#' #Check if discrepancies were solved in the dataset
#' attr(bf_data, "solve_discrepancies")
#' #Solve discrepancies
#' bf_solved <- solve_discrepancies(bf_data)
#' #Check if discrepancies were solved in the dataset
#' attr(bf_solved, "solve_discrepancies")
solve_discrepancies <- function(data) {
  if (missing(data)) {
    stop("Argument data is not defined")
  }
  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ",
                class(data)))
  }

  if(isTRUE(attr(data, "solve_discrepancies"))) {
    stop("Any discrepancies have already been resolved in this dataset")
  }

  #Get varieties, subspecies (and forms) with accepted names that occurs in Brazil
  ind_spp <- which(data$taxonRank %in% c("Variety", "Subspecies", "Form") &
    data$taxonomicStatus == "Accepted" &
    data$endemism != "Not_found_in_Brazil")
  spp_var <- data[ind_spp, "species"]
  #Create temporarily column with binomial species name
  spp_var_bin <- get_binomial(spp_var, include_variety = FALSE,
                              include_subspecies = FALSE)
  data$species_bin <- get_binomial(data$species, include_variety = FALSE,
                              include_subspecies = FALSE)
  #Get only species that exists as Species in dataframe
  spp_var_yes <- intersect(data$species_bin[which(
    data$taxonRank == "Species" & data$taxonomicStatus == "Accepted")],
                           spp_var_bin)
  #Other species
  spp_var_no <- na.omit(setdiff(na.omit(spp_var_bin),
                        data$species_bin[which(
    data$taxonRank == "Species")]))

  #Get dataframe to update
  d_upt <- subset(data, data$species_bin %in% spp_var_yes)

  #Update columns
  dd_updated_list <- lapply(split(d_upt, d_upt$species_bin), update_columns)

  # Merge dataframes
  d_upt2 <- do.call(rbind, dd_updated_list)
  row.names(d_upt) <- NULL

  #Update final dataframe
  data_solved <- rbind(subset(data, !(data$id %in% d_upt$id)), d_upt)

  #Fix varieties and subspecies that does not appear as species
  if(length(spp_var_no) > 0) {
  df_no_species <- data[data$species_bin %in% na.omit(spp_var_no),]
  #Change taxonrank
  df_no_species$taxonRank <- "Species"
  #Create new id
  df_no_species$id <- sample(setdiff(1:50000, data$id), nrow(df_no_species))
  #Subset columns
  df_no_species <- df_no_species[, colnames(data)]
  #Merge data
  data_solved <- rbind(data_solved, df_no_species)
  }

  #Add attribute
  attr(data_solved, "solve_discrepancies") <- TRUE

  #Remove Unknown when there are info
  columns <- c("lifeForm", "habitat", "biome",
               "states", "vegetation", "origin", "endemism")
  for(i in columns) {
    data_solved[[i]][which(
      grepl(";Unknown|Unknown;", data_solved[[i]]))] <- gsub(
        ";Unknown|Unknown;", "", data_solved[[i]][which(grepl(";Unknown|Unknown;",
                                                     data_solved[[i]]))])
  }

  #Remove column with binomial name without var or subsp
  data_solved$species_bin <- NULL

  #Return
  return(data_solved)
}
