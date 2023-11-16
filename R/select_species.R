#' Selection of species based on its characteristics and distribution
#'
#' @description select_species allows filter species based on its
#' characteristics and distribution available in Brazilian Flora 2020
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function.
#' @param include_subspecies (logical) include subspecies?
#' Default = FALSE
#' @param include_variety (logical) include varieties of the species?
#' Default = FALSE
#' @param Kingdom (character) The Kingdom for filtering the dataset. It can be
#' "Plantae" or "Fungi". Default = "Plantae". To include both,
#' use c("Plantae", "Fungi")
#' @param Group (character) The groups for filtering the datasets. It can be
#' "Fungi", "Angiosperms", "Gymnosperms", "Ferns and Lycophytes",
#' "Bryophytes" and "Algae". To use more than one group, put the available
#' items in a vector, for example: Group = c(Angiosperms", "Gymnosperms").
#' Default = "All".
#' @param Subgroup (character) The subgroups for filtering the dataset.
#' Only available if the Group is "Fungi" or "Bryophytes". For Fungi, it can be
#' "stricto sensu" or "lato sensu". For Bryophytes, it can be "Mosses",
#' "Hornworts" and "Liverworts" . To use more than one group, put the available
#' items in a vector, for example: Subgroup = c("Mosses", "Hornworts").
#' Default = "All".
#' @param Family (character) The families for filtering the dataset. It can
#' be included more than one Family. Default = "All".
#' @param Genus (character) The genus for filtering the dataset. It can
#' be included more than one Genus. Default = "All".
#' @param LifeForm (character) The life forms for filtering the dataset. It can
#' be included more than one LifeForm. Default = "All"
#' @param filter_LifeForm (character) The type of filtering for life forms. It
#' can be "in", "only", "not_in" and "and". See details for more about this
#' argument.
#' @param Habitat (character) The life habitat for filtering the dataset. It can
#' be included more than one habitat. Default = "All"
#' @param filter_Habitat (character) The type of filtering for habitat. It
#' can be "in", "only", "not_in" and "and". See details for more about this
#' argument.
#' @param Biome (character) The biomes for filtering the dataset. It can
#' be included more than one biome. Default = "All"
#' @param filter_Biome (character) The type of filtering for biome. It
#' can be "in", "only", "not_in" and "and". See details for more about this
#' argument.
#' @param State (character) The States for filtering the dataset. It can
#' be included more than one state. Default = "All".
#' @param filter_State (character) The type of filtering for state. It
#' can be "in", "only", "not_in" and "and". See Details for more about this
#' argument.
#' @param VegetationType (character) The vegetation types for filtering the
#' dataset. It can be included more than one vegetation type. Default = "All".
#' @param filter_Vegetation (character) The type of filtering for
#' vegetation type. It can be "in", "only", "not_in" and "and". See details for
#' more about this argument.
#' @param Endemism (character) The endemism (endemic or non-endemic to Brazil)
#' for filtering the dataset. It can be "All", "Endemic" or "Non-endemic".
#' Default = "All".
#' @param Origin (character) The origin for filtering the dataset. It can
#' be "All", "Native", "Cultivated" and "Naturalized". Default = "All".
#' @param TaxonomicStatus (character) The taxonomic status for filtering the
#' dataset. It can be "All", "Accepted" or "Synonym". Default = "Accepted".
#' @param NomenclaturalStatus (character) The nomenclatural status for
#' filtering the dataset. Default = "Accepted"
#'
#' @details It's possible to choose 4 ways to filter by lifeform, by habitat,
#' by biome, by state and by vegetation type:
#' "in": selects species that have any occurrence of the determined values. It
#' allows multiple matches. For example, if Biome = c("Amazon", Cerrado" and
#' filter_Biome = "in", it will select all species that occur in the Amazon and
#' Cerrado, some of which may also occur in other biomes.
#'
#' "only": selects species that have only occurrence of the determined values.
#' It allows only single matches. For example, if Biome = c("Amazon", "Cerrado")
#' and filter_Biome = "only", it will select all species that occur exclusively
#' in both the Amazon and Cerrado biomes, without any occurrences in other
#' biomes.
#'
#' "not_in": selects species that don't have occurrence of the determined
#' values. It allows single and multiple matches. For example,
#' if Biome = c("Amazon", "Cerrado") and filter_Biome = "not_in", it will select
#' all species without occurrences in the Amazon and Cerrado biomes.
#'
#' "and": selects species that have occurrence in all determined values. It
#' allows single and multiple matches. For example,
#' if Biome = c("Amazon", "Cerrado") and filter_Biome = "and", it will select
#' all species that occurs only in both the Amazon and Cerrado biomes,
#' including species that occurs in other biomes too.
#'
#'
#'
#' To get the complet list of arguments available for Family, Genus, LifeForm,
#' Habitat, Biome, State, and NomenclaturalStatus, use the function
#' \code{\link{get_attributes}}
#'
#'
#' @return A new dataframe with the filtered species.
#' @usage select_species(data,
#'                       include_subspecies = FALSE, include_variety = FALSE,
#'                       Kingdom = "Plantae", Group = "All", Subgroup = "All",
#'                       Family = "All", Genus = "All",
#'                       LifeForm = "All", filter_LifeForm = "in",
#'                       Habitat = "All", filter_Habitat = "in",
#'                       Biome = "All", filter_Biome = "in",
#'                       State = "All", filter_State = "in",
#'                       VegetationType = "All", filter_Vegetation = "in",
#'                       Endemism = "All", Origin = "All",
#'                       TaxonomicStatus = "Accepted",
#'                       NomenclaturalStatus = "All")
#' @export
#' @references
#' Brazilian Flora 2020. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#'
#' @examples
#' data("bf_data") #Load Brazilian Flora data
#' #'Select endemic and native species of trees with disjunct occurrence in
#' # Atlantic Forest and Amazon
#' am_af_only <- select_species(data = bf_data,
#'                              include_subspecies = FALSE,
#'                              include_variety = FALSE,
#'                              Kingdom = "Plantae",
#'                              Group = "All", Subgroup = "All",
#'                              Family = "All", Genus = "All",
#'                              LifeForm = "Tree", filter_LifeForm = "only",
#'                              Habitat = "All", filter_Habitat = "in",
#'                              Biome = c("Atlantic_Forest","Amazon"),
#'                              filter_Biome = "only",
#'                              State = "All", filter_State = "and",
#'                              VegetationType = "All", filter_Vegetation = "in",
#'                              Endemism = "Endemic", Origin = "Native",
#'                              TaxonomicStatus = "All",
#'                              NomenclaturalStatus = "All")

select_species <- function(data,
                           include_subspecies = FALSE,
                           include_variety = FALSE,
                           Kingdom = "Plantae",
                           Group = "All", Subgroup = "All",
                           Family = "All",
                           Genus = "All",
                           LifeForm = "All", filter_LifeForm = "in",
                           Habitat = "All", filter_Habitat = "in",
                           Biome = "All", filter_Biome = "in",
                           State = "All", filter_State = "in",
                           VegetationType = "All", filter_Vegetation = "in",
                           Endemism = "All", Origin = "All",
                           TaxonomicStatus = "Accepted",
                           NomenclaturalStatus = "All") {
  if (missing(data)) {
    stop("Argument data is not defined")
  }

  #Check classes
  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ", class(data)))
  }

  if (!is.logical(include_subspecies)) {
    stop(paste0("Argument include_subspecies must be logical, not ",
                class(include_subspecies)))
  }

  if (!is.logical(include_variety)) {
    stop(paste0("Argument include_variety must be logical, not ",
                class(include_variety)))
  }


  if(!(filter_LifeForm %in% c("in", "only", "not_in", "and"))) {
    stop(paste0("Argument filter_LifeForm must be:\n",
                "'in', 'only', 'not_in' or 'and'"))
  }
  if(!(filter_Habitat %in% c("in", "only", "not_in", "and"))) {
    stop(paste0("Argument filter_Habitat must be:\n",
                "'in', 'only', 'not_in' or 'and'"))
  }
  if(!(filter_Biome %in% c("in", "only", "not_in", "and"))) {
    stop(paste0("Argument filter_Biome must be:\n",
                "'in', 'only', 'not_in' or 'and'"))
  }
  if(!(filter_Vegetation %in% c("in", "only", "not_in", "and"))) {
    stop(paste0("Argument filter_Vegetation must be:\n",
                "'in', 'only', 'not_in' or 'and'"))
  }

  if(!(Kingdom %in% unique(data$kingdom))) {
    stop(paste("Kingdom not valid. The Kingdoms availables are:\n",
               paste(unique(data$kingdom), collapse = ", ")))  }

  if(all(Group != "All") & !all(Group %in% unique(data$Group))) {
    stop(paste("Group not valid. The Groups availables are:\n",
               paste(unique(data$Group), collapse = ", ")))  }

  if(Subgroup != "All" & !(Subgroup %in% unique(data$Subgroup))) {
    stop(paste("Subgroup not valid.
               The subgroups are only available for non-Plants:\n",
               paste(na.omit(unique(data$Subgroup)), collapse = ", ")))  }

  if(all(Family != "All") & !all(Family %in% unique(data$family))) {
    stop(paste("Family not valid.\n",
    "Check the available families with the function get_attributes()")) }

  if(Genus != "All" & !(Genus %in% unique(data$genus))) {
    stop(paste("Genus not valid.\n")) }

  if(Endemism != "All" & !(Endemism %in% unique(data$Endemism))) {
    stop(paste("Endemism not valid. The options availables are:\n",
               "'All', 'Endemic', 'Non-endemic', or NA"))}
  if(Origin != "All" & !(Origin %in% unique(data$Origin))) {
    stop(paste("Origin not valid. The options availables are:\n",
               "'All', 'Native', 'Cultivated', 'Naturalized', or NA"))}
  if(TaxonomicStatus != "All" & !(TaxonomicStatus %in% unique(data$taxonomicStatus))) {
    stop(paste("Origin not valid. The options availables are:\n",
               "'All', 'Accepted', 'Synonym', or NA"))}
  if(NomenclaturalStatus != "All" &
     !(NomenclaturalStatus %in% unique(data$nomenclaturalStatus))) {
    stop(paste("NomenclaturalStatus not valid.\n",
               "Check the available NomenclaturalStatus with the function\n",
               "get_attributes()")) }

  #Start to filter...

  #Kingdom
  d <- subset(data, data$kingdom %in% Kingdom)

  #Taxon Rank
  if(!include_subspecies & !include_variety) {
    d <- subset(d, d$taxonRank == "Species") }
  if(include_subspecies & !include_variety) {
    d <- subset(d, d$taxonRank %in% c("Species", "Subspecies")) }
  if(!include_subspecies & include_variety) {
    d <- subset(d, d$taxonRank %in% c("Species", "Variety")) }
  if(include_subspecies & include_variety) {
    d <- subset(d, d$taxonRank %in% c("Species", "Subspecies", "Variety")) }

  #Group
  if(all(Group == "All")) {
    d <- d } else {
      gr <- paste(Group, collapse = "|")
      d <- subset(d, grepl(gr, d$Group))
    }

  #Subgroup
  if(all(Subgroup == "All")) {
    d <- d } else {
      subgr <- paste(Subgroup, collapse = "|")
      d <- subset(d, grepl(subgr, d$Subgroup))
    }

  #Subgroup
  if(Subgroup == "All") {
    d <- d } else {
      d <- subset(d, d$Subgroup %in% Subgroup)
    }

    #Family
  if(all(Family == "All")) {
    d <- d } else {
    ffam <- paste(Family, collapse = "|")
    d <- subset(d, grepl(ffam, d$family))
    }

    #Genus
  if(all(Genus == "All")) {
    d <- d } else {
      ggen <- paste(Genus, collapse = "|")
      d <- subset(d, grepl(ggen, d$genus))
    }

   #LifeForm ####
  if(all(LifeForm  == "All")) {
    d <- d }

  #Check if it is a valid LifeForm
  if(all(LifeForm != "All")) {
    all_lf <- unique(unlist(strsplit(d$lifeForm, split = ";")))
    newLifeForm <- gsub(" ", "", LifeForm)
    newLifeForm <- sapply(LifeForm, function(x){
      paste(sort(gsub(" ", "", unlist(strsplit(x, split = ",")))),collapse = ";")
    }, USE.NAMES = FALSE)
    newLifeForm <- sort(newLifeForm)
    #Check if all lifeform exists
    newLifeForm2 <- unique(unlist(strsplit(newLifeForm, split = ";")))
    any_diff <- setdiff(newLifeForm2 , all_lf)
    if(length(any_diff) > 0) {
      warning(paste("The following life forms are not valid:\n",
                 paste(any_diff, collapse = ", ")))
      }
  }

  #Filter by lifeform
  if(all(LifeForm != "All") & filter_LifeForm == "in") {
    d <- subset(d, grepl(paste(newLifeForm, collapse = "|"),
                         d$lifeForm)) }

  if(all(LifeForm != "All") & filter_LifeForm == "only") {
    d <- subset(d, d$lifeForm == paste(newLifeForm, collapse = ";"))
  }

  if(all(LifeForm != "All") & filter_LifeForm == "not_in") {
    d <- subset(d, !grepl(paste(newLifeForm, collapse = "|"),
                          d$lifeForm))

  }

  if(all(LifeForm != "All") & filter_LifeForm == "and") {
    d <- subset(d, grepl(paste(newLifeForm, collapse = ";"), d$lifeForm ))
  }


  #Habitat ####
  if(all(Habitat  == "All")) {
    d <- d }

  #Check if it is a valid Habitat
  if(all(Habitat != "All")) {
    all_hab <- unique(unlist(strsplit(d$habitat, split = ";")))
    newHabitat <- gsub(" ", "", Habitat)
    newHabitat <- sapply(Habitat, function(x){
      paste(sort(gsub(" ", "", unlist(strsplit(x, split = ",")))),collapse = ";")
    }, USE.NAMES = FALSE)
    newHabitat <- sort(newHabitat)
    #Check if all Habitat exists
    newHabitat2 <- unique(unlist(strsplit(newHabitat, split = ";")))
    any_diff <- setdiff(newHabitat2 , all_hab)
    if(length(any_diff) > 0) {
      warning(paste("The following life forms are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
  }

  #Filter by Habitat
  if(all(Habitat != "All") & filter_Habitat == "in") {
    d <- subset(d, grepl(paste(newHabitat, collapse = "|"),
                         d$habitat)) }

  if(all(Habitat != "All") & filter_Habitat == "only") {
    d <- subset(d, d$habitat == paste(newHabitat, collapse = ";"))
  }

  if(all(Habitat != "All") & filter_Habitat == "not_in") {
    d <- subset(d, !grepl(paste(newHabitat, collapse = "|"),
                          d$habitat))

  }

  if(all(Habitat != "All") & filter_Habitat == "and") {
    d <- subset(d, grepl(paste(newHabitat, collapse = ";"), d$habitat))
  }

  #Biome ####
  if(all(Biome  == "All")) {
    d <- d}

  #Check if it is a valid Biome
  if(all(Biome != "All")) {
    all_biome <- unique(unlist(strsplit(d$Biome, split = ";")))
    newBiome <- gsub(" ", "", Biome)
    newBiome <- sapply(Biome, function(x){
      paste(sort(gsub(" ", "", unlist(strsplit(x, split = ",")))),collapse = ";")
    }, USE.NAMES = FALSE)
    newBiome <- sort(newBiome)
    #Check if all Biome exists
    newBiome2 <- unique(unlist(strsplit(newBiome, split = ";")))
    any_diff <- setdiff(newBiome2 , all_biome)
    if(length(any_diff) > 0) {
      warning(paste("The following life forms are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
  }

  #Filter by Biome
  if(all(Biome != "All") & filter_Biome == "in") {
    d <- subset(d, grepl(paste(newBiome, collapse = "|"),
                         d$Biome)) }

  if(all(Biome != "All") & filter_Biome == "only") {
    d <- subset(d, d$Biome == paste(newBiome, collapse = ";"))
  }

  if(all(Biome != "All") & filter_Biome == "not_in") {
    d <- subset(d, !grepl(paste(newBiome, collapse = "|"),
                          d$Biome))
  }

  if(all(Biome != "All") & filter_Biome == "and") {
    d <- subset(d, grepl(paste(newBiome, collapse = ";"), d$Biome ))
  }

  #State ####
  if(all(State  == "All")) {
    d <- d}

  #Check if it is a valid State
  if(all(State != "All")) {
    all_State <- unique(unlist(strsplit(d$States, split = ";")))
    newState <- gsub(" ", "", State)
    newState <- sapply(State, function(x){
      paste(sort(gsub(" ", "", unlist(strsplit(x, split = ",")))),collapse = ";")
    }, USE.NAMES = FALSE)
    newState <- sort(newState)
    #Check if all State exists
    newState2 <- unique(unlist(strsplit(newState, split = ";")))
    any_diff <- setdiff(newState2 , all_State)
    if(length(any_diff) > 0) {
      warning(paste("The following States are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
  }

  #Filter by State
  if(all(State != "All") & filter_State == "in") {
    d <- subset(d, grepl(paste(newState, collapse = "|"),
                           d$States)) }

  if(all(State != "All") & filter_State == "only") {
    d <- subset(d, d$States == paste(newState, collapse = ";"))
  }

  if(all(State != "All") & filter_State == "not_in") {
    d <- subset(d, !grepl(paste(newState, collapse = "|"),
                          d$States))

  }

  if(all(State != "All") & filter_State == "and") {
    d <- subset(d, grepl(paste(newState, collapse = ";"), d$States))
    }

  #Vegetation ####
  if(all(VegetationType  == "All")) {
    d <- d}

  #Check if it is a valid Vegetation
  if(all(VegetationType != "All")) {
    all_Vegetation <- unique(unlist(strsplit(d$vegetationType, split = ";")))
    newVegetation <- gsub(" ", "", VegetationType)
    newVegetation <- sapply(newVegetation, function(x){
      paste(sort(gsub(" ", "", unlist(strsplit(x, split = ",")))),collapse = ";")
    }, USE.NAMES = FALSE)
    newVegetation <- sort(newVegetation)
    #Check if all Vegetation exists
    newVegetation2 <- unique(unlist(strsplit(newVegetation, split = ";")))
    any_diff <- setdiff(newVegetation2 , all_Vegetation)
    if(length(any_diff) > 0) {
      warning(paste("The following vegetation types are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
  }

  #Filter by Vegetation
  if(all(VegetationType != "All") & filter_Vegetation == "in") {
    d <- subset(d, grepl(paste(newVegetation2, collapse = "|"),
                         d$vegetationType)) }

  if(all(VegetationType != "All") & filter_Vegetation == "only") {
    d <- subset(d, d$vegetationType == paste(newVegetation2, collapse = ";"))
  }

  if(all(VegetationType != "All") & filter_Vegetation == "not_in") {
    d <- subset(d, !grepl(paste(newVegetation2, collapse = "|"),
                          d$vegetationType))

  }

  if(all(VegetationType != "All") & filter_Vegetation == "and") {
    d <- subset(d, grepl(paste(newVegetation2, collapse = ";"), d$vegetationType))
  }

  #Endemism ####
  if(all(Endemism == "All")) {
    d <- d }

  #Filter by Endemism
  if(all(Endemism != "All")) {
    Endemism2 <- Endemism
    all_endemism <- unique(d$Endemism)
    any_diff <- setdiff(Endemism, all_endemism)
    if(length(any_diff) > 0) {
      warning(paste("The following endemisms are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
    d <- subset(d, d$Endemism == Endemism2) }

  #Origin ####
  if(all(Origin == "All")) {
    d <- d }

  #Filter by Origin
  if(all(Origin != "All")) {
    Origin2 <- Origin
    all_Origin <- unique(d$Origin)
    any_diff <- setdiff(Origin, all_Origin)
    if(length(any_diff) > 0) {
      warning(paste("The following origins are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
    d <- subset(d, d$Origin %in% Origin2) }

  #taxonomicStatus ####
  if(all(TaxonomicStatus == "All")) {
    d <- d }

  #Filter by TaxonomicStatus
  if(all(TaxonomicStatus != "All")) {
    TaxonomicStatus2 <- TaxonomicStatus
    all_taxonomicStatus <- unique(d$taxonomicStatus)
    any_diff <- setdiff(TaxonomicStatus, all_taxonomicStatus)
    if(length(any_diff) > 0) {
      warning(paste("The following TaxonomicStatuss are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
    d <- subset(d, d$taxonomicStatus %in% TaxonomicStatus2) }

  #nomenclaturalStatus ####
  if(all(NomenclaturalStatus == "All")) {
    d <- d }

  #Filter by nomenclaturalStatus
  if(all(NomenclaturalStatus != "All")) {
    nomenclaturalStatus2 <- NomenclaturalStatus
    all_nomenclaturalStatus <- unique(d$nomenclaturalStatus)
    any_diff <- setdiff(nomenclaturalStatus2, all_nomenclaturalStatus)
    if(length(any_diff) > 0) {
      warning(paste("The following nomenclaturalStatuss are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
    d <- subset(d, d$nomenclaturalStatus %in% nomenclaturalStatus2) }

  if(nrow(d) == 0) {
    warning("Combination of characteristics return 0 species")
    }

  return(d)
  } #End of function
