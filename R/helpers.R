#####Helper functions####

#First letter to upper case
firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

firstup_collapse <- function(x, collapse = "_") {
  res <- sapply(x, function(text){
    # Split words
    words <- strsplit(text, collapse)[[1]]
    # First letter to upper case
    words <- sapply(words, firstup)
    # Merge words
    text_final <- paste(words, collapse = collapse)
  })
  names(res) <- NULL
  return(res)
}

#Extract string between patterns
extract_between <- function(str, left, right) {
  inicio <- regexpr(left, str) + attr(regexpr(left, str), "match.length")
  str_inicio <- substring(str, first = inicio)
  fim <- regexpr(right, str_inicio)
  final <- substring(str_inicio, first = 0, last = fim - 1)
  ifelse(inicio > 0 & fim > 0, final, NA)
}


#Translate lifeform from portuguese to english
translate_lifeform <- function(lifeform) {
  newlifeform <- gsub("Aquatica-Bentos", "Aquatic-benthos", lifeform)
  newlifeform <- gsub("Aquatica-Neuston", "Aquatic-neuston", newlifeform)
  newlifeform <- gsub("Aquatica-Plancton", "Aquatic-plankton", newlifeform)
  newlifeform <- gsub("Arbusto", "Shrub", newlifeform)
  newlifeform <- gsub("Arvore", "Tree", newlifeform)
  newlifeform <- gsub("Bambu", "Bamboo", newlifeform)
  newlifeform <- gsub("Coxim", "Cushion", newlifeform)
  newlifeform <- gsub("Dendroide", "Dendroid", newlifeform)
  newlifeform <- gsub("Desconhecida", "Unknown", newlifeform)
  newlifeform <- gsub("Dracenoide", "Dracaenoid", newlifeform)
  newlifeform <- gsub("Endofitico", "Endophyte", newlifeform)
  newlifeform <- gsub("Entomogeno", "Entomogenous", newlifeform)
  newlifeform <- gsub("Erva", "Herb", newlifeform)
  newlifeform <- gsub("Flabelado", "Flabellate", newlifeform)
  newlifeform <- gsub("Folhosa", "Foliose", newlifeform)
  newlifeform <- gsub("Liana/voluvel/trepadeira", "Liana/scandent/vine",
                      newlifeform)
  newlifeform <- gsub("Liquenizado", "Lichenized", newlifeform)
  newlifeform <- gsub("Micorrizico", "Mycorrhizal", newlifeform)
  newlifeform <- gsub("Palmeira", "Palm_tree", newlifeform)
  newlifeform <- gsub("Parasita", "Parasite", newlifeform)
  newlifeform <- gsub("Pendente", "Pendent", newlifeform)
  newlifeform <- gsub("Saprobio", "Saprobe", newlifeform)
  newlifeform <- gsub("Subarbusto", "Subshrub", newlifeform)
  newlifeform <- gsub("Suculenta", "Succulent", newlifeform)
  newlifeform <- gsub("Talosa", "Thallose", newlifeform)
  newlifeform <- gsub("Tapete", "Mat", newlifeform)
  newlifeform <- gsub("Trama", "Weft", newlifeform)
  newlifeform <- gsub("Tufo", "Tuft", newlifeform)


  return(newlifeform)
}

#Translate habitat from portuguese to english
translate_habitat <- function(habitat) {
  newhabitat <- gsub("Agua", "Water", habitat)
  newhabitat <- gsub("Animal morto", "Dead_animal", newhabitat)
  newhabitat <- gsub("Animal vivo", "Living_animal", newhabitat)
  newhabitat <- gsub("Aquatica", "Aquatic", newhabitat)
  newhabitat <- gsub("Areia", "Sand", newhabitat)
  newhabitat <- gsub("Corticicola", "Corticolous", newhabitat)
  newhabitat <- gsub("Desconhecido", "Unknown", newhabitat)
  newhabitat <- gsub("Edafica", "Edaphic", newhabitat)
  newhabitat <- gsub("Epifila", "Epiphyllous", newhabitat)
  newhabitat <- gsub("Epifita", "Epiphytic", newhabitat)
  newhabitat <- gsub("Epixila", "Epixilous", newhabitat)
  newhabitat <- gsub("Esterco ou Fezes", "Dung_or_feces", newhabitat)
  newhabitat <- gsub("Folhedo a?reo", "Aerial_litter", newhabitat)
  newhabitat <- gsub("Folhedo submerso", "Submerged_litter", newhabitat)
  newhabitat <- gsub("Hemiepifita", "Hemiepiphyte", newhabitat)
  newhabitat <- gsub("Hemiparasita", "Hemiparasite", newhabitat)
  newhabitat <- gsub("Outro", "Other", newhabitat)
  newhabitat <- gsub("Parasita", "Parasite", newhabitat)
  newhabitat <- gsub("Planta viva - cortex do caule",
                     "Living_plant_stem_cortex",
                     newhabitat)
  newhabitat <- gsub("Planta viva - cortex galho", "Living_plant_branch_cortex",
                     newhabitat)
  newhabitat <- gsub("Planta viva - folha", "Living_plant_leaf", newhabitat)
  newhabitat <- gsub("Planta viva - fruto", "Living_plant_fruit", newhabitat)
  newhabitat <- gsub("Planta viva - inflorescencia",
                     "Living_plant_inflorescence", newhabitat)
  newhabitat <- gsub("Planta viva - raiz", "Living_plant_root", newhabitat)
  newhabitat <- gsub("Rocha", "Rock", newhabitat)
  newhabitat <- gsub("Rupicola", "Rupicolous", newhabitat)
  newhabitat <- gsub("Saprofita", "Saprophyte", newhabitat)
  newhabitat <- gsub("Saxicola", "Saxicolous", newhabitat)
  newhabitat <- gsub("Semente", "Seed", newhabitat)
  newhabitat <- gsub("Simbionte \\(incluindo fungos liquenizados\\)",
                     "Symbiont", newhabitat)
  newhabitat <- gsub("Solo", "Soil", newhabitat)
  newhabitat <- gsub("Sub-aerea", "Subaerial", newhabitat)
  newhabitat <- gsub("Terricola", "Terrestrial", newhabitat)
  newhabitat <- gsub("Tronco em decomposicao", "Decaying_wood", newhabitat)
  newhabitat <- gsub("Planta viva", "Living_plant", newhabitat)
  newhabitat <- gsub("Folhedo", "Leaf_litter", newhabitat)
  newhabitat <- gsub("Outro fungo", "Another_fungus", newhabitat)
  return(newhabitat)
  }

#Translate biome from portuguese to english
translate_biome <- function(biome) {
  newbiome <- gsub("Amazonia", "Amazon", biome)
  newbiome <- gsub("Mata Atlantica", "Atlantic_Forest", newbiome)
  newbiome <- gsub("Nao ocorre no Brasil", "Not_found_in_brazil", newbiome)
  return(newbiome)
}

#Translate vegetation from portuguese to english
translate_vegetation <- function(vegetation) {
  newvegetation <- gsub("Area Antropica", "Anthropic_Area", vegetation)
  newvegetation <- gsub("Caatinga \\(stricto sensu\\)", "Caatinga",
                        newvegetation)
  newvegetation <- gsub("Campinarana", "Amazonian_Campinarana", newvegetation)
  newvegetation <- gsub("Campo de Altitude", "High_Altitude_Grassland",
                        newvegetation)
  newvegetation <- gsub("Campo de Varzea", "Flooded_Field", newvegetation)
  newvegetation <- gsub("Campo Limpo", "Grassland", newvegetation)
  newvegetation <- gsub("Campo rupestre", "Highland_Rocky_Field", newvegetation)
  newvegetation <- gsub("Carrasco", "Carrasco", newvegetation)
  newvegetation <- gsub("Cerrado \\(lato sensu\\)", "Cerrado", newvegetation)
  newvegetation <- gsub("Floresta Ciliar ou Galeria", "Gallery_Forest",
                        newvegetation)
  newvegetation <- gsub("Floresta de Igapo", "Inundated_Forest_Igapo",
                        newvegetation)
  newvegetation <- gsub("Floresta de Terra Firme", "Terra_Firme_Forest",
                        newvegetation)
  newvegetation <- gsub("Floresta de Varzea", "Inundated_Forest", newvegetation)
  newvegetation <- gsub("Floresta Estacional Decidual",
                        "Seasonallly_Deciduous_Forest", newvegetation)
  newvegetation <- gsub("Floresta Estacional Perenifolia",
                        "Seasonal_Evergreen_Forest", newvegetation)
  newvegetation <- gsub("Floresta Estacional Semidecidual",
                        "Seasonally_Semideciduous_Forest", newvegetation)
  newvegetation <- gsub("Floresta Ombrofila \\(= Floresta Pluvial\\)",
                        "Rainforest", newvegetation)
  newvegetation <- gsub("Floresta Ombrofila Mista",
                        "Mixed_Ombrophyllous_Forest", newvegetation)
  newvegetation <- gsub("Manguezal", "Mangrove", newvegetation)
  newvegetation <- gsub("Palmeiral", "Palm_Grove", newvegetation)
  newvegetation <- gsub("Restinga", "Restinga", newvegetation)
  newvegetation <- gsub("Savana Amazonica", "Amazonian_Savanna", newvegetation)
  newvegetation <- gsub("Vegetacao Aquatica", "Aquatic_Vegetation",
                        newvegetation)
  newvegetation <- gsub("Vegetacao Sobre Afloramentos Rochosos",
                        "Rock_Outcrop_Vegetation", newvegetation)
  newvegetation <- gsub("Nao ocorre no Brasil", "Not_found_in_brazil",
                        newvegetation)
  return(newvegetation)
}

#Translate endemism from portuguese to english
translate_endemism <- function(endemism) {
  newendemism <- ifelse(endemism == "", "Unknown",
                      ifelse(endemism == "Nao endemica", "Non-endemic",
                              ifelse(endemism == "Endemica", "Endemic",
                                    ifelse (endemism == "Nao ocorre no Brasil",
                                            "Not_found_in_brazil", NA))))

  return(newendemism)
}

#Translate origin from portuguese to english
translate_origin <- function(origin) {
  neworigin <- ifelse(origin == "", "Unknown",
                 ifelse(origin == "NATIVA", "Native",
                        ifelse(origin == "CULTIVADA", "Cultivated",
                              ifelse(origin == "NATURALIZADA", "Naturalized",
                                        ifelse(origin == "Nao ocorre no Brasil",
                                              "Not_found_in_brazil", NA)))))

  return(neworigin)
}

#Translate nomenclatural status
translate_nomenclaturalStatus <- function(status) {
  newstatus <- status
  newstatus[which(newstatus == "NOME_CORRETO")] <- "Correct"
  newstatus[which(newstatus ==
                  "NOME_LEGITIMO_MAS_INCORRETO")] <- "Legitimate_but_incorrect"
  newstatus[which(newstatus ==
              "NOME_CORRETO_VIA_CONSERVACAO")] <- "Correct_name_by_conservation"
  newstatus[which(newstatus ==
                    "VARIANTE_ORTOGRAFICA")] <- "Orthographical_variant"
  newstatus[which(newstatus == "NOME_ILEGITIMO")] <- "Illegitimate"
  newstatus[which(newstatus ==
              "NOME_NAO_EFETIVAMENTE_PUBLICADO")] <- "Not_effectively_published"
  newstatus[which(newstatus ==
                  "NOME_NAO_VALIDAMENTE_PUBLICADO")] <- "Not_validly_published"
  newstatus[which(newstatus ==
                    "NOME_APLICACAO_INCERTA")] <- "Uncertain_Application"
  newstatus[which(newstatus == "NOME_REJEITADO")] <- "Rejected"
  newstatus[which(newstatus == "NOME_MAL_APLICADO")] <- "Misapplied"
  return(newstatus)}

#Translate taxonomic status
translate_taxonomicStatus <- function(status){
  newstatus <- status
  newstatus[which(newstatus == "NOME_ACEITO")] <- "Accepted"
  newstatus[which(newstatus == "SINONIMO")] <- "Synonym"
  return(newstatus)
}

#Translate taxon rank
translate_taxonRank <- function(taxonRank){
  newrank <- taxonRank
  newrank[which(newrank== "ORDEM")] <- "Order"
  newrank[which(newrank== "FAMILIA")] <- "Family"
  newrank[which(newrank== "GENERO")] <- "Genus"
  newrank[which(newrank== "ESPECIE")] <- "Species"
  newrank[which(newrank== "VARIEDADE")] <- "Variety"
  newrank[which(newrank== "SUB_ESPECIE")] <- "Subspecies"
  newrank[which(newrank== "CLASSE")] <- "Class"
  newrank[which(newrank== "TRIBO")] <- "Tribe"
  newrank[which(newrank== "SUB_FAMILIA")] <- "Subfamily"
  newrank[which(newrank== "DIVISAO")] <- "Division"
  newrank[which(newrank== "FORMA")] <- "Form"
  return(newrank)
}

#Translate group
translate_group <- function(group){
  newgroup <- group
  newgroup[which(newgroup == "Fungos")] <- "Fungi"
  newgroup[which(newgroup == "Angiospermas")] <- "Angiosperms"
  newgroup[which(newgroup == "Gimnospermas")] <- "Gymnosperms"
    newgroup[which(newgroup ==
                     "Samambaias e Licofitas")] <- "Ferns and Lycophytes"
  newgroup[which(newgroup == "Briofitas")] <- "Bryophytes"
  newgroup[which(newgroup == "Algas")] <- "Algae"
  return(newgroup)
 }

#Translate subgroup
translate_subgroup <- function(subgroup){
  newsubgroup <- subgroup
  newsubgroup[which(newsubgroup == "Antoceros")] <- "Hornworts"
  newsubgroup[which(newsubgroup == "Hepaticas")] <- "Liverworts"
  newsubgroup[which(newsubgroup == "Musgos")] <- "Mosses"
  return(newsubgroup)
}

#Solve discrepancies between varieties/subspecies and species
update_columns <- function(df) {
  # Get unique values of lifeForm, habitat, vegetation, biome e states
  unique_lifeForm <- sort(unique(unlist(strsplit(df$lifeForm, ";"))))
  unique_habitat <- sort(unique(unlist(strsplit(df$habitat, ";"))))
  unique_vegetation <- sort(unique(unlist(strsplit(df$vegetation, ";"))))
  unique_biome <- sort(unique(unlist(strsplit(df$biome, ";"))))
  unique_states <- sort(unique(unlist(strsplit(df$states, ";"))))

  # Update columns where taxonRank == "Species"
  df$lifeForm[df$taxonRank == "Species"] <- paste(unique_lifeForm, collapse = ";")
  df$habitat[df$taxonRank == "Species"] <- paste(unique_habitat, collapse = ";")
  df$vegetation[df$taxonRank == "Species"] <- paste(unique_vegetation, collapse = ";")
  df$biome[df$taxonRank == "Species"] <- paste(unique_biome, collapse = ";")
  df$states[df$taxonRank == "Species"] <- paste(unique_states, collapse = ";")

  #Return only taxonRank == "Species"
  df_final <- subset(df, df$taxonRank == "Species")

  return(df_final)
}

merge_data <- function(path_data, version_data, solve_discrepancy,
                       encoding = "UTF-8", verbose) {

  #Set folder
  if(is.null(path_data)) {
    stop(paste("Argument path_data is not defined, this is necessary for",
               "\n saving data"))
  }

  #Print message
  if(verbose) {
    message("Data will be saved in ", path_data, "\n") }

  #Get latest available version if version was not set
  if(version_data == "latest") {
    all_dirs <- list.dirs(path = path_data, recursive = FALSE,
                          full.names = FALSE)
    dir_versions <- subset(all_dirs, grepl("393", all_dirs)) #Actual version
    #Get highest version
    if(length(dir_versions) > 0) {
      high_version <- max(as.numeric(gsub("393.", "", dir_versions)))
      version_data <- paste0("393.", high_version) } else {
        version_data <- 0
      } }

  #Taxon
  taxon <- utils::read.csv(file.path(path_data, version_data, "taxon.txt"),
                           header=TRUE, sep = "\t",
                           encoding = encoding, na.strings = "")
  #Remove accents
  taxon$higherClassification <- iconv(taxon$higherClassification,
                                      to="ASCII//TRANSLIT")

  #Vernacular name
  vernacular <- utils::read.csv(file.path(path_data, version_data,
                                          "vernacularname.txt"),
                                header=TRUE, sep = "\t",
                                encoding = encoding, na.strings = "")
  #Remove accents
  vernacular$vernacularName <- iconv(vernacular$vernacularName,
                                     to="ASCII//TRANSLIT")

  #group vernacular names from same species
  grouped <- split(vernacular, vernacular$id)
  summarized <- lapply(grouped, function(group) {
    paste(group$vernacularName, collapse = ", ")
  })
  vernacular_final <- data.frame(
    id = as.numeric(names(summarized)),
    vernacularName = unlist(summarized)
  )

  ###Species Profile
  spProfile <- utils::read.csv(file.path(path_data, version_data,
                                         "speciesprofile.txt"),
                               header=TRUE, sep = "\t",
                               encoding = encoding, na.strings = "")
  #Remove accents
  spProfile$lifeForm <- iconv(spProfile$lifeForm, to="ASCII//TRANSLIT")
  spProfile$habitat <- iconv(spProfile$habitat, to="ASCII//TRANSLIT")


  #Extract informations to new columns
  #Life form
  spProfile$lifeForm.new <- extract_between(spProfile$lifeForm,
                                            left = "lifeForm:\\[",
                                            right = "\\]")
  #Habitat
  spProfile$habitat.new <- extract_between(spProfile$lifeForm,
                                           left = "habitat:\\[",
                                           right = "\\]")
  #Vegetation type
  spProfile$vegetation.new <- extract_between(spProfile$lifeForm,
                                                  left = "vegetationType:\\[",
                                                  right = "\\]")

  #Rename and select columns
  spProfile <- spProfile[,c("id", "lifeForm.new", "habitat.new",
                            "vegetation.new")]
  colnames(spProfile) <- c("id", "lifeForm", "habitat", "vegetation")

  ###Distribution and Location
  dist <- utils::read.csv(file.path(path_data, version_data,
                                    "distribution.txt"),
                          header=TRUE, sep = "\t",
                          encoding = encoding, na.strings = "")
  #Remove accents
  dist$occurrenceRemarks <- iconv(dist$occurrenceRemarks,
                                  to="ASCII//TRANSLIT")
  #Extrair informações para novas coluna
  #origin
  dist$origin <- dist$establishmentMeans
  #endemism
  dist$endemism <- ifelse(grepl("endemism:Nao endemica",
                                dist$occurrenceRemarks),
                          "Nao endemica",
                          ifelse(grepl("endemism:Endemica",
                                       dist$occurrenceRemarks),
                                 "Endemica", NA))
  #Phytogeographic domain
  dist$phytogeographicDomain <- extract_between(dist$occurrenceRemarks,
                                                left = "phytogeographicDomain:\\[",
                                                right = "\\]")

  #Deletar aspas
  dist$phytogeographicDomain <- gsub("\"", "", dist$phytogeographicDomain)
  #locationID - State
  dist$locationID <- gsub(".*-", "", dist$locationID)

  #Organize information
  #Local
  Local <- dist[,c("id","locationID","countryCode")]
  #group location of same species
  grouped <- split(Local, Local$id)
  summarized <- lapply(grouped, function(group) {
    paste(group$locationID, collapse = ";")
  })
  Local_final <- data.frame(
    id = as.numeric(names(summarized)),
    locationID = unlist(summarized)
  )
  #Merge distribution data again
  dist_final <- dist[, c("id", "countryCode", "origin", "endemism",
                         "phytogeographicDomain")]
  dist_final <- merge(dist_final, Local_final, by = "id")
  dist_final <- unique(dist_final[,colnames(dist_final)])

  #Merge all information
  df_final1 <- merge(taxon, vernacular_final, by = "id", all = TRUE)
  df_final2 <- merge(df_final1, spProfile, by = "id", all = TRUE)
  df_final3 <- merge(df_final2, dist_final, by = "id", all = TRUE)

  #Create columns with name of the specie and accepted name
  df_final3$species <- NA
  #Subspecies and varieties rank
  subvar_rank <- c("VARIEDADE", "SUB_ESPECIE")
  #Species
  df_final3$species[which(df_final3$taxonRank == "ESPECIE")] <- paste(
    df_final3$genus[which(df_final3$taxonRank == "ESPECIE")],
    df_final3$specificEpithet[which(df_final3$taxonRank == "ESPECIE")]
  )
  #Varieties
  df_final3$species[which(df_final3$taxonRank == "VARIEDADE")] <- paste(
    df_final3$genus[which(df_final3$taxonRank == "VARIEDADE")],
    df_final3$specificEpithet[which(df_final3$taxonRank == "VARIEDADE")],
    "var.",
    df_final3$infraspecificEpithet[which(df_final3$taxonRank == "VARIEDADE")]
  )
  #Varieties
  df_final3$species[which(df_final3$taxonRank == "SUB_ESPECIE")] <- paste(
    df_final3$genus[which(df_final3$taxonRank == "SUB_ESPECIE")],
    df_final3$specificEpithet[which(df_final3$taxonRank == "SUB_ESPECIE")],
    "subsp.",
    df_final3$infraspecificEpithet[which(df_final3$taxonRank == "SUB_ESPECIE")]
  )

    # Old version of binomial extraction
    # gsub("^([[:alnum:]]+[-[:alnum:]]*(?:[[:space:]]+[[:alnum:]]+[-[:alnum:]]*)?)\\b.*",
    #      "\\1",
    #      df_final3$scientificName[which(!(df_final3$taxonRank %in%
    #                                         ignore_rank))])

  # df_final3$species[which(!(df_final3$taxonRank %in% ignore_rank))] <-
  #   gsub("^((\\w+\\W+){1}\\w+).*$","\\1",
  #        df_final3$scientificName[which(!(df_final3$taxonRank %in%
  #                                           ignore_rank))])

  #Accepted name when is synonymn
  ignore_rank <- na.omit(setdiff(unique(df_final3$taxonRank),
                         c("ESPECIE", "VARIEDADE", "SUB_ESPECIE")))
  df_final3$acceptedName <- NA
  df_final3$acceptedName[which(!(df_final3$taxonRank %in%
                                   ignore_rank))] <- get_binomial(species_names =
                                     df_final3$acceptedNameUsage[which(
                                       !(df_final3$taxonRank %in%
                                           ignore_rank))])


    # gsub("^((\\w+\\W+){1}\\w+).*$","\\1",
    # df_final3$acceptedNameUsage[which(!(df_final3$taxonRank %in%
    #                                       ignore_rank))])

  #Get group and subgroup
  #group
  df_final3$group <- extract_between(str = df_final3$higherClassification,
                                     left = ";", right = ";")
  df_final3$group <- translate_group(group = df_final3$group)
  #subgroup - Only Bryophytes and Fungi
  df_final3$subgroup <- NA
  df_final3$subgroup[which(df_final3$group ==
                             "Bryophytes")] <- extract_between(
                               str = df_final3$higherClassification[which(
                                 df_final3$group == "Bryophytes")],
                               left = "Briofitas;",
                               right = ";")
  df_final3$subgroup[which(df_final3$group == "Fungi")] <- extract_between(
    str = df_final3$higherClassification[which(df_final3$group == "Fungi")],
    left = "Fungos;",
    right = ";")
  df_final3$subgroup <- translate_subgroup(subgroup = df_final3$subgroup)

  #Order columns
  df_final <- df_final3[,c(c("id", "taxonID", "acceptedNameUsageID",
                             "parentNameUsageID", "originalNameUsageID",
                             "group", "subgroup",
                             "species",
                             "acceptedName", "scientificName",
                             "acceptedNameUsage",
                             "parentNameUsage",
                             "namePublishedIn",  "namePublishedInYear",
                             "higherClassification", "kingdom",
                             "phylum", "class", "order", "family", "genus",
                             "specificEpithet",
                             "infraspecificEpithet", "taxonRank",
                             "scientificNameAuthorship",
                             "taxonomicStatus", "nomenclaturalStatus",
                             "vernacularName", "lifeForm",
                             "habitat", "vegetation",
                             "origin", "endemism", "phytogeographicDomain",
                             "locationID",
                             "countryCode", "modified", "bibliographicCitation",
                             "references"))]
  #Accepted names that are not in the species column...
  #Species that do not occurs in Brazil
  sp_out <- setdiff(unique(df_final$acceptedName),
                    unique(df_final$species))
  sp_out_df <- subset(df_final, df_final$acceptedName %in% sp_out)
  #Identify duplicates
  sp_out_dup <- duplicated(sp_out_df[, c("acceptedName")])
  sp_out_unique <- sp_out_df[!sp_out_dup, ]
  #Update distribution - Unknown
  sp_out_unique$vegetation <- "Not_found_in_brazil"
  sp_out_unique$endemism <- "Not_found_in_brazil"
  sp_out_unique$origin <- "Not_found_in_brazil"
  sp_out_unique$locationID <- "Not_found_in_brazil"
  sp_out_unique$phytogeographicDomain <- "Not_found_in_brazil"
  #Update taxonomic info
  sp_out_unique$species <- get_binomial(sp_out_unique$acceptedName,
                                        include_variety = FALSE,
                                        include_subspecies = FALSE)
  sp_out_unique$scientificName <- sp_out_unique$acceptedNameUsage
  sp_out_unique$nomenclaturalStatus <- "NOME_CORRETO"
  sp_out_unique$taxonomicStatus <- "NOME_ACEITO"
  sp_out_unique$genus <- gsub( " .*$", "", sp_out_unique$species)
  sp_out_unique$specificEpithet <- gsub( ".* ", "", sp_out_unique$species)
  sp_out_unique$id <- sp_out_unique$acceptedNameUsageID
  sp_out_unique$taxonID <- sp_out_unique$acceptedNameUsageID
  sp_out_unique$taxonRank <- "Species"
  #Join info
  df_join <- rbind(df_final, sp_out_unique)

  #Translate
  df_join$lifeForm <- translate_lifeform(lifeform = df_join$lifeForm)
  df_join$habitat <- translate_habitat(habitat = df_join$habitat)
  df_join$phytogeographicDomain <- translate_biome(biome =
                                                     df_join$phytogeographicDomain)
  df_join$vegetation <- translate_vegetation(vegetation =
                                                   df_join$vegetation)
  df_join$endemism <- translate_endemism(endemism = df_join$endemism)
  df_join$origin <- translate_origin(origin = df_join$origin)
  df_join$taxonomicStatus <- translate_taxonomicStatus(status =
                                                         df_join$taxonomicStatus)
  df_join$nomenclaturalStatus <- translate_nomenclaturalStatus(status =
                                                                 df_join$nomenclaturalStatus)
  df_join$taxonRank <- translate_taxonRank(taxonRank = df_join$taxonRank)


  #Sort information and separe using ;
  df_join$lifeForm <- vapply(df_join$lifeForm, FUN.VALUE = character(1),
                             function(x){
                               paste(sort(unlist(strsplit(x, split = ","))),collapse = ";")
                             }, USE.NAMES = FALSE)
  df_join$habitat <- vapply(df_join$habitat, FUN.VALUE = character(1),
                            function(x){
                              paste(sort(unlist(strsplit(x, split = ","))),collapse = ";")
                            }, USE.NAMES = FALSE)
  df_join$phytogeographicDomain <- vapply(df_join$phytogeographicDomain,
                                          FUN.VALUE = character(1),
                                          function(x){
                                            paste(sort(unlist(strsplit(x, split = ","))),collapse = ";")
                                          }, USE.NAMES = FALSE)
  df_join$locationID <- vapply(df_join$locationID,
                               FUN.VALUE = character(1),
                               function(x){
                                 paste(sort(unlist(strsplit(x, split = ";"))),collapse = ";")
                               }, USE.NAMES = FALSE)
  df_join$vegetation <- vapply(df_join$vegetation ,
                                  FUN.VALUE = character(1),
                                  function(x){
                                    paste(sort(unlist(strsplit(x, split = ","))),collapse = ";")
                                  }, USE.NAMES = FALSE)

  #Replace space by underline in biome and vegetation
  df_join$phytogeographicDomain <- gsub(" ", "_", df_join$phytogeographicDomain)
  df_join$vegetation <- gsub(" ", "_", df_join$vegetation)
  df_join$endemism <- gsub(" ", "_", df_join$endemism)
  df_join$origin <- gsub(" ", "_", df_join$origin)

  #Rename columns
  colnames(df_join)[colnames(df_join) == "phytogeographicDomain"] <- "biome"
  colnames(df_join)[colnames(df_join) == "locationID"] <- "states"


  # if(solve_incongruences){
  #   #Solve incongruences between species and subspecies
  #   #Get varieties, subspecies (and forms) with accepted names that occurs in Brazil
  #   spp_var <- subset(df_join,
  #                     df_join$taxonRank %in% c("Variety", "Subspecies", "Form") &
  #                       df_join$taxonomicStatus == "Accepted" &
  #                       df_join$endemism != "Not_found_in_brazil")[["species"]]
  #
  #   #Get only species that exists as Species in dataframe
  #   spp_var_yes <- intersect(df_join$species[which(df_join$taxonRank == "Species")],
  #                            spp_var)
  #   spp_var_no <- setdiff(spp_var, df_join$species[which(df_join$taxonRank == "Species")])
  #
  #   #Get dataframe to update
  #   d_upt <- subset(df_join, df_join$species %in% spp_var_yes)
  #
  #   #Update columns
  #   dd_updated_list <- lapply(split(d_upt, d_upt$species), update_columns)
  #
  #   # Merge dataframes
  #   d_upt <- do.call(rbind, dd_updated_list)
  #   row.names(d_upt) <- NULL
  #
  #   #Update final dataframe
  #   df_join <- rbind(subset(df_join, !(df_join$id %in% d_upt$id)), d_upt)
  #
  #   #Fix varieties and subspecies that does not appear as species
  #   df_no_species <- subset(df_join, df_join$species %in% spp_var_no)
  #   #Change taxonrank
  #   df_no_species$taxonRank <- "Species"
  #   #Create new id
  #   df_no_species$id <- sample(setdiff(1:50000, df_join$id), nrow(df_no_species))
  #   #Merge data
  #   df_join <- rbind(df_join, df_no_species)
  #   }

  if(solve_discrepancy){
    df_solved <- solve_discrepancies(df_join)

    #Fill NAs
    df_solved <- fill_NA(df_solved)

    #Save
    saveRDS(df_solved,
            file = file.path(path_data, version_data,
                             "CompleteBrazilianFlora.rds"))
  } else {
  #Save as RDS
  #Fill NAs
  df_join <- fill_NA(df_join)
  attr(df_join, "solve_discrepancies") <- FALSE
  saveRDS(df_join,
          file = file.path(path_data, version_data,
                           "CompleteBrazilianFlora.rds"))
  }
}

#Fill NAs and empty values with Unknown
fill_NA <- function(data){
  #taxon ranks to fix
  tr <- c("Species", "Subspecies", "Variety")

  #Replace empty space by NA
  for(i in c("lifeForm", "habitat", "biome", "states", "vegetation")) {
    data[[i]][which(data[[i]] == "" | data[[i]] == "NA")] <- NA
  }

  #Fill NA with "Unknown"
  for(i in c("lifeForm", "habitat", "biome", "states", "vegetation",
             "endemism", "origin")) {
    data[[i]][which(is.na(data[[i]]) & data$taxonRank %in% tr &
                      data$taxonomicStatus == "Accepted" &
                      (data$biome != "Not_found_in_brazil" |
                         is.na(data$biome)))] <- "Unknown"
  }
  for(i in c("lifeForm", "habitat", "biome", "states", "vegetation",
             "endemism", "origin")) {
    data[[i]][which(is.na(data[[i]]) & data$taxonRank %in% tr &
                      data$taxonomicStatus == "Accepted" &
                      data$biome == "Not_found_in_brazil")] <- "Not_found_in_brazil"
  }

  return(data)
}

#Extract varieties
extract_varieties <- function(species) {
  varieties <- sub(".*var\\.\\s+(\\w+).*", "\\1", species)
  return(varieties)
}

#Extract subspecies
extract_subspecies <- function(species) {
  subspecies <- sub(".*subsp\\.\\s+(\\w+).*", "\\1", species)
  return(subspecies)
}

# ####Generate data to filter_florabR#####
# library(dplyr)
# library(data.table)
# library(terra)
# library(geobr)

# ####Get Flora do Brazil dataset####
# my_dir <- "../BrazilianFlora"
# dir.create(my_dir)
# get_florabr(output_dir = my_dir)
#
# #Flora do Brazil data
# df <- load_florabr(data_dir = my_dir, type = "short")
# #Get only species and Plantae
# p <- df %>%
#   filter(kingdom == "Plantae", taxonRank %in% c("Species", "Variety" , "Subspecies"))
# #Get only accepted names
# pac <- p %>% filter(taxonomicStatus == "Accepted")
# #Subset some species
# bf_data <- pac
# usethis::use_data(bf_data, overwrite = TRUE)
#
#
# ####Get species occurrences####
# library(plantR)
# library(CoordinateCleaner)
# library(pbapply)
# library(dplyr)
#
# spp <- c("Araucaria angustifolia", "Abatia americana", "Passiflora edmundoi",
#          "Myrcia hatschbachii", "Serjania pernambucensis", "Inga virescens",
#          "Solanum restingae")
#
# oc.gbif <- pblapply(spp, function(i) {
#   rgbif2(species = i, force = TRUE, remove_na = TRUE) })
# oc.gbif <- bind_rows(oc.gbif)
#
#
# #Clean data
# library(CoordinateCleaner)
# oc_n <- oc.gbif %>% mutate(decimalLatitude = as.numeric(decimalLatitude),
#                            decimalLongitude = as.numeric(decimalLongitude))
#
# occ_f <- clean_coordinates(x = oc_n, lon = "decimalLongitude",
#                            lat = "decimalLatitude",
#                            species = "species", countries = "countryCode",
#                            tests = c("capitals", "centroids", "equal", "gbif",
#                                      "institutions","seas", "zeros"))
# #Select only valid records
# occ <- occ_f %>% filter(.summary == TRUE) %>%
#   dplyr::select(species, x = "decimalLongitude", y = "decimalLatitude",
#                 datasetKey) #To get DOI
# #Remove duplicates
# occ_dup <- cc_dupl(occ, species = "species", lon = "x", lat = "y")
# occurrences <- data.frame(occ_dup)
# #Data set key
# ds_key <- occurrences %>% count(datasetKey)
#
# derived_dataset(
#   citation_data = ds_key,
#   title = "florabr R package: Records of plant species",
#   description="This data was downloaded using plantR::rgbif2, filtered using
#   CoordinateCleaner::clean_coordinates and later incorported as data example in
#   florabr R Package",
#   source_url="https://github.com/wevertonbio/florabr/raw/main/data/occurrences.rda",
#   gbif_download_doi = NULL,
#   user = user, #User in GBIF
#   pwd = pwd) #Password in GBIF
#
# #Remove datasetKey column
# occurrences <- occurrences %>% dplyr::select(-datasetKey)
# usethis::use_data(occurrences, overwrite = TRUE)
