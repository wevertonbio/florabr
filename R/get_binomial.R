#' Extract the binomial name (Genus + specific epithet) from a Scientific Name
#'
#' @param species_names (character) Scientific names to be converted to
#' binomial names
#'
#' @return A vector with the binomial names (Genus + specific epithet).
#' @usage get_binomial(species_names)
#' @export
#'
#' @examples
#' spp <- c("Araucaria angustifolia (Bertol.) Kuntze",
#' "Butia catarinensis Noblick & Lorenzi",
#' "Adesmia paranensis Burkart")
#' spp_new <- get_binomial(species_names = spp)
#' spp_new
#'
get_binomial <- function(species_names) {
  if (!is.character(species_names)) {
    stop(paste0("Argument species_names must be a character, not ",
                class(species_names)))
  }

  # Remove excess of whitespace between words
  species_names <- gsub("\\s+", " ", species_names)

  # Remove leading and/or trailing whitespace
  species_names <- trimws(species_names)


  selected_species_names <- vapply(species_names, FUN.VALUE = character(1),
                                   function(text) {
    words <- strsplit(text, " ")[[1]]
    word_count <- length(words)

    if (word_count > 2) {
      selected_words <- paste(words[1:2], collapse = " ")
      return(selected_words)
    } else {
      return(text)
    }
  })
  names(selected_species_names) <- NULL
  return(selected_species_names)
}

