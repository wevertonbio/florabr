#' Selection of species based on its characteristics and distribution
#'
#' @description select_species allows filter species based on its
#' characteristics and distribution available in Flora e Funga do Brasil
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function.
#' @param include_subspecies (logical) include subspecies?
#' Default = FALSE
#' @param include_variety (logical) include varieties of the species?
#' Default = FALSE
#' @param kingdom (character) The kingdom for filtering the dataset. It can be
#' "Plantae" or "Fungi". Default = "Plantae". To include both,
#' use c("Plantae", "Fungi")
#' @param group (character) The groups for filtering the datasets. It can be
#' "Fungi", "Angiosperms", "Gymnosperms", "Ferns and Lycophytes",
#' "Bryophytes" and "Algae". To use more than one group, put the available
#' items in a vector, for example: group = c(Angiosperms", "Gymnosperms").
#' Default = "All".
#' @param subgroup (character) The subgroups for filtering the dataset.
#' Only available if the group is "Fungi" or "Bryophytes". For Fungi, it can be
#' "stricto sensu" or "lato sensu". For Bryophytes, it can be "Mosses",
#' "Hornworts" and "Liverworts" . To use more than one group, put the available
#' items in a vector, for example: subgroup = c("Mosses", "Hornworts").
#' Default = "All".
#' @param phylum (character) The phyla for filtering the dataset. It can
#' be included more than one phylum. Default = "All".
#' @param class (character) The classes for filtering the dataset. It can
#' be included more than one class. Default = "All".
#' @param order (character) The orders for filtering the dataset. It can
#' be included more than one order. Default = "All".
#' @param family (character) The families for filtering the dataset. It can
#' be included more than one family. Default = "All".
#' @param genus (character) The genus for filtering the dataset. It can
#' be included more than one genus. Default = "All".
#' @param lifeForm (character) The life forms for filtering the dataset. It can
#' be included more than one lifeForm. Default = "All"
#' @param filter_lifeForm (character) The type of filtering for life forms. It
#' can be "in", "only", "not_in" and "and". See details for more about this
#' argument.
#' @param habitat (character) The life habitat for filtering the dataset. It can
#' be included more than one habitat. Default = "All"
#' @param filter_habitat (character) The type of filtering for habitat. It
#' can be "in", "only", "not_in" and "and". See details for more about this
#' argument.
#' @param biome (character) The biomes for filtering the dataset. It can
#' be included more than one biome. Default = "All"
#' @param filter_biome (character) The type of filtering for biome. It
#' can be "in", "only", "not_in" and "and". See details for more about this
#' argument.
#' @param state (character) The states for filtering the dataset. It can
#' be included more than one state. Default = "All".
#' @param filter_state (character) The type of filtering for state. It
#' can be "in", "only", "not_in" and "and". See Details for more about this
#' argument.
#' @param vegetation (character) The vegetation types for filtering the
#' dataset. It can be included more than one vegetation type. Default = "All".
#' @param filter_vegetation (character) The type of filtering for
#' vegetation type. It can be "in", "only", "not_in" and "and". See details for
#' more about this argument.
#' @param endemism (character) The endemism (endemic or non-endemic to Brazil)
#' for filtering the dataset. It can be "All", "Endemic" or "Non-endemic".
#' Default = "All".
#' @param origin (character) The origin for filtering the dataset. It can
#' be "All", "Native", "Cultivated" and "Naturalized". Default = "All".
#' @param taxonomicStatus (character) The taxonomic status for filtering the
#' dataset. It can be "All", "Accepted" or "Synonym". Default = "Accepted".
#' @param nomenclaturalStatus (character) The nomenclatural status for
#' filtering the dataset. Default = "Accepted"
#'
#' @details It's possible to choose 4 ways to filter by lifeForm, by habitat,
#' by biome, by state and by vegetation type:
#' "in": selects species that have any occurrence of the determined values. It
#' allows multiple matches. For example, if biome = c("Amazon", Cerrado" and
#' filter_biome = "in", it will select all species that occur in the Amazon and
#' Cerrado, some of which may also occur in other biomes.
#'
#' "only": selects species that have only occurrence of the determined values.
#' It allows only single matches. For example, if biome = c("Amazon", "Cerrado")
#' and filter_biome = "only", it will select all species that occur exclusively
#' in both the Amazon and Cerrado biomes, without any occurrences in other
#' biomes.
#'
#' "not_in": selects species that don't have occurrence of the determined
#' values. It allows single and multiple matches. For example,
#' if biome = c("Amazon", "Cerrado") and filter_biome = "not_in", it will select
#' all species without occurrences in the Amazon and Cerrado biomes.
#'
#' "and": selects species that have occurrence in all determined values. It
#' allows single and multiple matches. For example,
#' if biome = c("Amazon", "Cerrado") and filter_biome = "and", it will select
#' all species that occurs only in both the Amazon and Cerrado biomes,
#' including species that occurs in other biomes too.
#'
#'
#'
#' To get the complete list of arguments available for family, genus, lifeForm,
#' habitat, biome, state, and nomenclaturalStatus, use the function
#' \code{\link{get_attributes}}
#'
#'
#' @return A new dataframe with the filtered species.
#' @usage select_species(data,
#'                       include_subspecies = FALSE, include_variety = FALSE,
#'                       kingdom = "Plantae", group = "All", subgroup = "All",
#'                       phylum = "All", class ="All", order = "All",
#'                       family = "All", genus = "All",
#'                       lifeForm = "All", filter_lifeForm = "in",
#'                       habitat = "All", filter_habitat = "in",
#'                       biome = "All", filter_biome = "in",
#'                       state = "All", filter_state = "in",
#'                       vegetation = "All", filter_vegetation = "in",
#'                       endemism = "All", origin = "All",
#'                       taxonomicStatus = "Accepted",
#'                       nomenclaturalStatus = "All")
#' @export
#' @references
#' Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#'
#' @examples
#' data("bf_data") #Load Flora e Funga do Brasil data
#' #'Select endemic and native species of trees with disjunct occurrence in
#' # Atlantic Forest and Amazon
#' am_af_only <- select_species(data = bf_data,
#'                              include_subspecies = FALSE,
#'                              include_variety = FALSE,
#'                              kingdom = "Plantae",
#'                              group = "All", subgroup = "All",
#'                              phylum = "All", class ="All", order = "All",
#'                              family = "All", genus = "All",
#'                              lifeForm = "Tree", filter_lifeForm = "only",
#'                              habitat = "All", filter_habitat = "in",
#'                              biome = c("Atlantic_Forest","Amazon"),
#'                              filter_biome = "only",
#'                              state = "All", filter_state = "and",
#'                              vegetation = "All",
#'                              filter_vegetation = "in",
#'                              endemism = "Endemic", origin = "Native",
#'                              taxonomicStatus = "All",
#'                              nomenclaturalStatus = "All")

select_species <- function(data,
                           include_subspecies = FALSE,
                           include_variety = FALSE,
                           kingdom = "Plantae",
                           group = "All", subgroup = "All",
                           phylum = "All", class ="All", order = "All",
                           family = "All",
                           genus = "All",
                           lifeForm = "All", filter_lifeForm = "in",
                           habitat = "All", filter_habitat = "in",
                           biome = "All", filter_biome = "in",
                           state = "All", filter_state = "in",
                           vegetation = "All", filter_vegetation = "in",
                           endemism = "All", origin = "All",
                           taxonomicStatus = "Accepted",
                           nomenclaturalStatus = "All") {
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


  if(!(filter_lifeForm %in% c("in", "only", "not_in", "and"))) {
    stop(paste0("Argument filter_lifeForm must be:\n",
                "'in', 'only', 'not_in' or 'and'"))
  }
  if(!(filter_habitat %in% c("in", "only", "not_in", "and"))) {
    stop(paste0("Argument filter_habitat must be:\n",
                "'in', 'only', 'not_in' or 'and'"))
  }
  if(!(filter_biome %in% c("in", "only", "not_in", "and"))) {
    stop(paste0("Argument filter_biome must be:\n",
                "'in', 'only', 'not_in' or 'and'"))
  }
  if(!(filter_vegetation %in% c("in", "only", "not_in", "and"))) {
    stop(paste0("Argument filter_vegetation must be:\n",
                "'in', 'only', 'not_in' or 'and'"))
  }

  if(!(kingdom %in% unique(data$kingdom))) {
    stop(paste("kingdom not valid. The kingdoms availables are:\n",
               paste(unique(data$kingdom), collapse = ", ")))  }

  if(all(group != "All") & !all(group %in% unique(data$group))) {
    stop(paste("group not valid. The groups availables are:\n",
               paste(unique(data$group), collapse = ", ")))  }

  if(subgroup != "All" & !(subgroup %in% unique(data$subgroup))) {
    stop(paste("subgroup not valid.
               The subgroups are only available for non-Plants:\n",
               paste(na.omit(unique(data$subgroup)), collapse = ", ")))  }

  if(all(family != "All") & !all(family %in% unique(data$family))) {
    stop(paste("family not valid.\n",
               "Check the available families with the function
               get_attributes()")
         ) }

  if(genus != "All" & !(genus %in% unique(data$genus))) {
    stop(paste("genus not valid.\n")) }

  if(endemism != "All" & !(endemism %in% c('All', 'Endemic', 'Non-endemic'))) {
    stop(paste("endemism not valid. The options availables are:\n",
               "'All', 'Endemic', or 'Non-endemic'"))}
  if(origin != "All" & !(origin %in% c('All', 'Native', 'Cultivated',
  'Naturalized'))) {
    stop(paste("origin not valid. The options availables are:\n",
               "'All', 'Native', 'Cultivated', or 'Naturalized'"))}
  if(taxonomicStatus != "All" &
     !(taxonomicStatus %in% c('All', 'Accepted', 'Synonym'))) {
    stop(paste("taxonomicStatus not valid. The options availables are:\n",
               "'All', 'Accepted', or 'Synonym'"))}
  if(nomenclaturalStatus != "All" &
     !(nomenclaturalStatus %in% c("Correct", "Legitimate_but_incorrect",
                                  "Correct_name_by_conservation",
                                  "Orthographical_variant",
                                  "Illegitimate", "Not_effectively_published",
                                  "Not_validly_published",
                                  "Uncertain_Application", "Rejected",
                                  "Misapplied"))) {
    stop(paste("nomenclaturalStatus not valid.\n",
               "Check the available nomenclaturalStatus with the function\n",
               "get_attributes()")) }

  #Start to filter...

  #kingdom
  #Make sure first letter of group is upper case
  kingdom <- firstup(trimws(kingdom))
  d <- subset(data, data$kingdom %in% kingdom)

  #Taxon Rank
  if(!include_subspecies & !include_variety) {
    d <- subset(d, d$taxonRank == "Species") }
  if(include_subspecies & !include_variety) {
    d <- subset(d, d$taxonRank %in% c("Species", "Subspecies")) }
  if(!include_subspecies & include_variety) {
    d <- subset(d, d$taxonRank %in% c("Species", "Variety")) }
  if(include_subspecies & include_variety) {
    d <- subset(d, d$taxonRank %in% c("Species", "Subspecies", "Variety")) }

  #group
  if(all(group == "All")) {
    d <- d } else {
      #Make sure first letter of group is upper case
      group <- firstup(trimws(group))
      if(group == "Ferns and lycophytes") {
        group <- "Ferns and Lycophytes"}
      gr <- paste(group, collapse = "|")
      d <- subset(d, grepl(gr, d$group))
    }

  # #subgroup
  # if(all(subgroup == "All")) {
  #   d <- d } else {
  #     #Make sure first letter of group is upper case
  #     subgroup <- firstup(trimws(subgroup))
  #     subgr <- paste(subgroup, collapse = "|")
  #     d <- subset(d, grepl(subgr, d$subgroup))
  #   }

  #subgroup
  if(subgroup == "All") {
    d <- d } else {
      #Make sure first letter is upper case
      subgroup_c <- firstup(trimws(subgroup))
      d <- subset(d, d$subgroup %in% subgroup_c)
    }

  #family
  if(all(family == "All")) {
    d <- d } else {
      #Make sure first letter  is upper case
      family_c <- firstup(trimws(family))
      d <- subset(d, d$family %in% family_c)
    }

  #phylum
  if(all(phylum == "All")) {
    d <- d } else {
      #Make sure first letter  is upper case
      phylum_c <- firstup(trimws(phylum))
      d <- subset(d, d$phylum %in% phylum_c)
    }

  #class
  if(all(class == "All")) {
    d <- d } else {
      #Make sure first letter  is upper case
      class_c <- firstup(trimws(class))
      d <- subset(d, d$class %in% class_c)
    }

  #order
  if(all(order == "All")) {
    d <- d } else {
      #Make sure first letter  is upper case
      order_c <- firstup(trimws(order))
      d <- subset(d, d$order %in% order_c)
    }

  #genus
  if(all(genus == "All")) {
    d <- d } else {
      #Make sure first letter is upper case
      genus_c <- firstup(trimws(genus))
      d <- subset(d, d$genus %in% genus_c)
    }

  #lifeForm ####
  if(all(lifeForm  == "All")) {
    d <- d }

  #Check if it is a valid lifeForm
  if(all(lifeForm != "All")) {
    #Make sure first letter of group is upper case
    newlifeForm<- firstup(trimws(lifeForm))
    all_lf <- unique(unlist(strsplit(d$lifeForm, split = ";")))
    newlifeForm <- sort(newlifeForm)
    # # newlifeForm <- gsub(" ", "", lifeForm)
    # # newlifeForm <- vapply(lifeForm, FUN.VALUE = character(1), function(x){
    # #   paste(sort(gsub(" ", "", unlist(strsplit(x, split = ",")))),
    # #         collapse = ";")
    # # }, USE.NAMES = FALSE)
    # #Check if all lifeForm exists
    # newlifeForm2 <- unique(unlist(strsplit(newlifeForm, split = ";")))
    any_diff <- setdiff(newlifeForm, all_lf)
    if(length(any_diff) > 0) {
      warning(paste("The following life forms are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
  }

  #Filter by lifeForm
  if(all(lifeForm != "All") & filter_lifeForm == "in") {
    d <- subset(d, grepl(paste(newlifeForm, collapse = "|"),
                         d$lifeForm)) }

  if(all(lifeForm != "All") & filter_lifeForm == "only") {
    d <- subset(d, d$lifeForm == paste(newlifeForm, collapse = ";"))
  }

  if(all(lifeForm != "All") & filter_lifeForm == "not_in") {
    d <- subset(d, !grepl(paste(newlifeForm, collapse = "|"),
                          d$lifeForm))

  }

  if(all(lifeForm != "All") & filter_lifeForm == "and") {
    d <- subset(d, grepl(paste(newlifeForm, collapse = ";"), d$lifeForm ))
  }


  #habitat ####
  if(all(habitat  == "All")) {
    d <- d }

  #Check if it is a valid habitat
  if(all(habitat != "All")) {
    #Make sure first letter is upper case
    newhabitat<- firstup(trimws(habitat))
    newhabitat <- sort(newhabitat)
    all_hab <- unique(unlist(strsplit(d$habitat, split = ";")))
    # newhabitat <- gsub(" ", "", habitat)
    # newhabitat <- vapply(habitat, FUN.VALUE = character(1), function(x){
    #   paste(sort(gsub(" ", "", unlist(strsplit(x, split = ",")))),
    #         collapse = ";")
    # }, USE.NAMES = FALSE)
    # newhabitat <- sort(newhabitat)
    #Check if all habitat exists
    # newhabitat2 <- unique(unlist(strsplit(newhabitat, split = ";")))
    any_diff <- setdiff(newhabitat , all_hab)
    if(length(any_diff) > 0) {
      warning(paste("The following habitats are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
  }

  #Filter by habitat
  if(all(habitat != "All") & filter_habitat == "in") {
    d <- subset(d, grepl(paste(newhabitat, collapse = "|"),
                         d$habitat)) }

  if(all(habitat != "All") & filter_habitat == "only") {
    d <- subset(d, d$habitat == paste(newhabitat, collapse = ";"))
  }

  if(all(habitat != "All") & filter_habitat == "not_in") {
    d <- subset(d, !grepl(paste(newhabitat, collapse = "|"),
                          d$habitat))

  }

  if(all(habitat != "All") & filter_habitat == "and") {
    d <- subset(d, grepl(paste(newhabitat, collapse = ";"), d$habitat))
  }

  #biome ####
  if(all(biome  == "All")) {
    d <- d}

  #Check if it is a valid biome
  if(all(biome != "All")) {
    #Make sure first letter is upper case
    newbiome <- firstup_collapse(trimws(biome))
    all_biome <- unique(unlist(strsplit(d$biome, split = ";")))
    newbiome <- sort(newbiome)
    # all_biome <- unique(unlist(strsplit(d$biome, split = ";")))
    # newbiome <- gsub(" ", "", biome)
    # newbiome <- vapply(biome, FUN.VALUE = character(1), function(x){
    #   paste(sort(gsub(" ", "", unlist(strsplit(x, split = ",")))),
    #         collapse = ";")
    # }, USE.NAMES = FALSE)
    # newbiome <- sort(newbiome)
    # #Check if all biome exists
    # newbiome2 <- unique(unlist(strsplit(newbiome, split = ";")))
    any_diff <- setdiff(newbiome, all_biome)
    if(length(any_diff) > 0) {
      warning(paste("The following biomes are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
  }

  #Filter by biome
  if(all(biome != "All") & filter_biome == "in") {
    d <- subset(d, grepl(paste(newbiome, collapse = "|"),
                         d$biome)) }

  if(all(biome != "All") & filter_biome == "only") {
    d <- subset(d, d$biome == paste(newbiome, collapse = ";"))
  }

  if(all(biome != "All") & filter_biome == "not_in") {
    d <- subset(d, !grepl(paste(newbiome, collapse = "|"),
                          d$biome))
  }

  if(all(biome != "All") & filter_biome == "and") {
    d <- subset(d, grepl(paste(newbiome, collapse = ";"), d$biome ))
  }

  #state ####
  if(all(state  == "All")) {
    d <- d}

  #Check if it is a valid state
  if(all(state != "All")) {
    #Make sure all letter is upper case
    newstate <- toupper(trimws(state))
    newstate <- sort(newstate)
    all_state <- unique(unlist(strsplit(d$states, split = ";")))
    # newstate <- gsub(" ", "", state)
    # newstate <- vapply(state, FUN.VALUE = character(1), function(x){
    #   paste(sort(gsub(" ", "", unlist(strsplit(x, split = ",")))),
    #         collapse = ";")
    # }, USE.NAMES = FALSE)
    # newstate <- sort(newstate)
    #Check if all state exists
    any_diff <- setdiff(newstate, all_state)
    if(length(any_diff) > 0) {
      warning(paste("The following states are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
  }

  #Filter by state
  if(all(state != "All") & filter_state == "in") {
    d <- subset(d, grepl(paste(newstate, collapse = "|"),
                         d$states)) }

  if(all(state != "All") & filter_state == "only") {
    d <- subset(d, d$states == paste(newstate, collapse = ";"))
  }

  if(all(state != "All") & filter_state == "not_in") {
    d <- subset(d, !grepl(paste(newstate, collapse = "|"),
                          d$states))

  }

  if(all(state != "All") & filter_state == "and") {
    d <- subset(d, grepl(paste(newstate, collapse = ";"), d$states))
  }

  #vegetation ####
  if(all(vegetation == "All")) {
    d <- d}

  #Check if it is a valid vegetation
  if(all(vegetation != "All")) {
    #Make sure first letter is upper case
    newvegetation<- firstup_collapse(trimws(vegetation))
    newvegetation <- sort(newvegetation)
    all_vegetation <- unique(unlist(strsplit(d$vegetation, split = ";")))
    # newvegetation <- gsub(" ", "", vegetation)
    # newvegetation <- vapply(newvegetation, FUN.VALUE = character(1),function(x){
    #   paste(sort(gsub(" ", "", unlist(strsplit(x, split = ",")))),
    #         collapse = ";")
    # }, USE.NAMES = FALSE)
    # newvegetation <- sort(newvegetation)
    #Check if all vegetation exists
    any_diff <- setdiff(newvegetation , all_vegetation)
    if(length(any_diff) > 0) {
      warning(paste("The following vegetation types are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
  }

  #Filter by vegetation
  if(all(vegetation != "All") & filter_vegetation == "in") {
    d <- subset(d, grepl(paste(newvegetation, collapse = "|"),
                         d$vegetation)) }

  if(all(vegetation != "All") & filter_vegetation == "only") {
    d <- subset(d, d$vegetation == paste(newvegetation, collapse = ";"))
  }

  if(all(vegetation != "All") & filter_vegetation == "not_in") {
    d <- subset(d, !grepl(paste(newvegetation, collapse = "|"),
                          d$vegetation))

  }

  if(all(vegetation != "All") & filter_vegetation == "and") {
    d <- subset(d, grepl(paste(newvegetation, collapse = ";"),
                         d$vegetation))
  }

  #endemism ####
  if(all(endemism == "All")) {
    d <- d }

  #Filter by endemism
  if(all(endemism != "All")) {
    #Make sure first letter is upper case
    newendemism <- firstup(trimws(endemism))
    all_endemism <- unique(d$endemism)
    any_diff <- setdiff(newendemism, all_endemism)
    if(length(any_diff) > 0) {
      warning(paste("The following endemisms are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
    d <- subset(d, d$endemism %in% newendemism) }

  #origin ####
  if(all(origin == "All")) {
    d <- d }

  #Filter by origin
  if(all(origin != "All")) {
    #Make sure first letter is upper case
    neworigin <- firstup(trimws(origin))
    all_origin <- unique(d$origin)
    any_diff <- setdiff(neworigin, all_origin)
    if(length(any_diff) > 0) {
      warning(paste("The following origins are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
    d <- subset(d, d$origin %in% neworigin) }

  #taxonomicStatus ####
  if(all(taxonomicStatus == "All")) {
    d <- d }

  #Filter by taxonomicStatus
  if(all(taxonomicStatus != "All")) {
    #Make sure first letter is upper case
    newtaxonomicStatus <- firstup(trimws(taxonomicStatus))
    all_taxonomicStatus <- unique(d$taxonomicStatus)
    any_diff <- setdiff(newtaxonomicStatus, all_taxonomicStatus)
    if(length(any_diff) > 0) {
      warning(paste("The following taxonomicStatus are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
    d <- subset(d, d$taxonomicStatus %in% newtaxonomicStatus) }

  #nomenclaturalStatus ####
  if(all(nomenclaturalStatus == "All")) {
    d <- d }

  #Filter by nomenclaturalStatus
  if(all(nomenclaturalStatus != "All")) {
    #Make sure first letter is upper case
    newnomenclaturalStatus <- firstup(trimws(nomenclaturalStatus))
    all_nomenclaturalStatus <- unique(d$nomenclaturalStatus)
    any_diff <- setdiff(newnomenclaturalStatus, all_nomenclaturalStatus)
    if(length(any_diff) > 0) {
      warning(paste("The following nomenclaturalStatus are not valid:\n",
                    paste(any_diff, collapse = ", ")))
    }
    d <- subset(d, d$nomenclaturalStatus %in% newnomenclaturalStatus) }

  if(nrow(d) == 0) {
    warning("Combination of characteristics return 0 species")
  }

  return(d)
} #End of function
