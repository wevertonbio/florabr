#' Retrieve synonyms for species
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function
#' @param species (character) names of the species
#' @param include_subspecies (logical) include subspecies that are synonyms of
#' the species? Default = TRUE
#' @param include_variety (logical) include varieties that are synonyms of the
#' species? Default = TRUE
#'
#' @return A data.frame containing unique synonyms of the specified species
#' along with relevant information on taxonomic and nomenclatural statuses.
#' @usage get_synonym(data, species,
#'                    include_subspecies = TRUE, include_variety = TRUE)
#' @export
#' @references
#' Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#' @examples
#' data("bf_data") #Load Flora e Funga do Brasil data
#' #Species to extract synonyms
#' spp <- c("Araucaria angustifolia", "Adesmia paranensis")
#' spp_synonyms <- get_synonym(data = bf_data, species = spp,
#'                             include_subspecies = TRUE,
#'                             include_variety = TRUE)
#' spp_synonyms
#'
get_synonym <- function(data, species,
                        include_subspecies = TRUE,
                        include_variety = TRUE){

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

  #Taxon Rank
  if(include_subspecies){
    ss <- "Subspecies"
  } else {ss <- NULL}

  if(include_variety){
    vs <- "Variety"
  } else {vs <- NULL}
  tr <- c("Species", ss, vs)

  data <- data[data$taxonRank %in% tr,]


  #Check if there is any species absent in d
  no_match <- setdiff(species, unique(data$species))
  if(length(no_match) > 0) {
    warning(paste("Some species are absent of Flora e Funga do Brasil database\n",
                  "Check the species names using the check_names() function"))
  }
  #Get match
  order <- setdiff(species, no_match)

  res <- unique(data[which(data$acceptedName %in% species),
                     c("acceptedName", "species", "taxonomicStatus",
                       "nomenclaturalStatus")])

  # res <- unique(data[which(get_binomial(data$acceptedName,
  #                                       include_variety = F,
  #                                       include_subspecies = F) %in% species),
  #                    c("acceptedName", "species", "taxonomicStatus",
  #                      "nomenclaturalStatus")])

  #Get species without synonyms
  no_syn <- setdiff(species, res$acceptedName)
  if(length(no_syn) > 0){
    res_no_syn <- data[data$species %in% no_syn,
                       c("acceptedName", "species", "taxonomicStatus",
                         "nomenclaturalStatus")]
    if(nrow(res_no_syn) == 0){
      res_no_syn <- data.frame("acceptedName" = no_syn,
                               "species" = "Not found",
                               "taxonomicStatus" = "Not found",
                               "nomenclaturalStatus" = "Not found")
    }
    # res_no_syn$acceptedName <- res_no_syn$species
    # res_no_syn$species <- NA
    res <- rbind(res, res_no_syn)
  }


  if(nrow(res) > 0) {
  #Reorder
  res <- res[order(match(res$acceptedName, order)), ]

  #Change name of the column
  colnames(res)[2] <- "synonym"

  #Remove accepted names
  res <- subset(res, !(res$synonym %in% species))

  return(res) } else {
    warning(paste("All specified species are absent of Flora e Funga do Brasil
                  database\n",
               "Check the species names using the check_names() function"))
    return(NULL)
  }

}
