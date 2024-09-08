#' Extract the binomial name (Genus + specific epithet + infraspecific epithet
#' (optional)) from a full Scientific Name
#'
#' @param species_names (character) Scientific names to be converted to
#' binomial names
#' @param include_subspecies (logical) include subspecies? If TRUE (default),
#' the function extracts any infraspecific epithet after the pattern "subsp."
#' @param include_variety (logical) include subspecies? If TRUE (default),
#' the function extracts any infraspecific epithet after the pattern "var."
#' @return A vector with the binomial names (Genus + specific epithet).
#' @usage get_binomial(species_names,
#'                    include_subspecies = TRUE,
#'                    include_variety = TRUE)
#' @export
#'
#' @examples
#' spp <- c("Araucaria angustifolia (Bertol.) Kuntze",
#'          "Araucaria angustifolia var. alba Reitz",
#'          "Butia catarinensis Noblick & Lorenzi",
#'          "Butia eriospatha subsp. punctata",
#'          "Adesmia paranensis Burkart")
#' spp_new <- get_binomial(species_names = spp,
#'                        include_subspecies = TRUE,
#'                        include_variety = TRUE)
#' spp_new
#'
get_binomial <- function(species_names,
                         include_subspecies = TRUE,
                         include_variety = TRUE) {
   if (!is.character(species_names)) {
     stop("Argument species_names must be a character")
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
      #Get variety?
      if(include_variety & grepl("var\\. ", text)){
        sp_var <- extract_varieties(text)
        selected_words <- paste(selected_words, "var.", sp_var)
      }

      if(include_subspecies & grepl("subsp\\. ", text)){
        sp_sub <- extract_subspecies(text)
        selected_words <- paste(selected_words, "subsp.",  sp_sub)
      }

      return(selected_words)
    } else {
      return(text)
    }
  })
  names(selected_species_names) <- NULL
  return(selected_species_names)
}

