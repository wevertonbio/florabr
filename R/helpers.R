#####Helper functions####

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
  newlifeform <- gsub("Aquatica-Bentos", "Aquatic-Benthos", lifeform)
  newlifeform <- gsub("Aquatica-Neuston", "Aquatic-Neuston", newlifeform)
  newlifeform <- gsub("Aquatica-Plancton", "Aquatic-Plankton", newlifeform)
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
                     "Living_plant-stem_cortex",
                     newhabitat)
  newhabitat <- gsub("Planta viva - cortex galho", "Living_plant-branch_cortex",
                     newhabitat)
  newhabitat <- gsub("Planta viva - folha", "Living plant-leaf", newhabitat)
  newhabitat <- gsub("Planta viva - fruto", "Living plant-fruit", newhabitat)
  newhabitat <- gsub("Planta viva - inflorescencia",
                     "Living_plant-inflorescence", newhabitat)
  newhabitat <- gsub("Planta viva - raiz", "Living_plant-root", newhabitat)
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
  newbiome <- gsub("Nao ocorre no Brasil", "Not_found_in_Brazil", newbiome)
  return(newbiome)
}

#Translate vegetationType from portuguese to english
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
                        "Rock_outcrop_Vegetation", newvegetation)
  newvegetation <- gsub("Nao ocorre no Brasil", "Not_found_in_Brazil",
                        newvegetation)
  return(newvegetation)
}

#Translate Endemism from portuguese to english
translate_Endemism <- function(Endemism) {
  newEndemism <- ifelse(Endemism == "", "Unknown",
                      ifelse(Endemism == "Nao endemica", "Non-endemic",
                              ifelse(Endemism == "Endemica", "Endemic",
                                    ifelse (Endemism == "Nao ocorre no Brasil",
                                            "Not_found_in_Brazil", NA))))

  return(newEndemism)
}

#Translate Origin from portuguese to english
translate_Origin <- function(Origin) {
  newOrigin <- ifelse(Origin == "", "Unknown",
                 ifelse(Origin == "NATIVA", "Native",
                        ifelse(Origin == "CULTIVADA", "Cultivated",
                              ifelse(Origin == "NATURALIZADA", "Naturalized",
                                        ifelse(Origin == "Nao ocorre no Brasil",
                                              "Not_found_in_Brazil", NA)))))

  return(newOrigin)
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

#Translate Group
translate_group <- function(Group){
  newGroup <- Group
  newGroup[which(newGroup == "Fungos")] <- "Fungi"
  newGroup[which(newGroup == "Angiospermas")] <- "Angiosperms"
  newGroup[which(newGroup == "Gimnospermas")] <- "Gymnosperms"
    newGroup[which(newGroup ==
                     "Samambaias e Licofitas")] <- "Ferns and Lycophytes"
  newGroup[which(newGroup == "Briofitas")] <- "Bryophytes"
  newGroup[which(newGroup == "Algas")] <- "Algae"
  return(newGroup)
 }

#Translate Subgroup
translate_subgroup <- function(Subgroup){
  newSubgroup <- Subgroup
  newSubgroup[which(newSubgroup == "Antoceros")] <- "Hornworts"
  newSubgroup[which(newSubgroup == "Hepaticas")] <- "Liverworts"
  newSubgroup[which(newSubgroup == "Musgos")] <- "Mosses"
  return(newSubgroup)
}

merge_data <- function(path_data, version_data,
                       encoding = "UTF-8", verbose = TRUE) {

  #Set folder
  if(is.null(path_data)) {
    stop(paste("Argument path_data is not defined, this is necessary for",
               "\n saving data"))
  }

  #Print message
  if(verbose) {
  message("Data will be saved in", path_data, "\n") }

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

  #Group vernacular names from same species
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
  spProfile$vegetationType.new <- extract_between(spProfile$lifeForm,
                                                  left = "vegetationType:\\[",
                                                  right = "\\]")

  #Rename and select columns
  spProfile <- spProfile[,c("id", "lifeForm.new", "habitat.new",
                            "vegetationType.new")]
  colnames(spProfile) <- c("id", "lifeForm", "habitat", "vegetationType")

  ###Distribution and Location
  dist <- utils::read.csv(file.path(path_data, version_data,
                                    "distribution.txt"),
                   header=TRUE, sep = "\t",
                   encoding = encoding, na.strings = "")
  #Remove accents
  dist$occurrenceRemarks <- iconv(dist$occurrenceRemarks,
                                  to="ASCII//TRANSLIT")
  #Extrair informações para novas coluna
  #Origin
  dist$Origin <- dist$establishmentMeans
  #Endemism
  dist$Endemism <- ifelse(grepl("endemism:Nao endemica",
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
  #Group location of same species
  grouped <- split(Local, Local$id)
  summarized <- lapply(grouped, function(group) {
    paste(group$locationID, collapse = ";")
  })
  Local_final <- data.frame(
    id = as.numeric(names(summarized)),
    locationID = unlist(summarized)
  )
  #Merge distribution data again
  dist_final <- dist[, c("id", "countryCode", "Origin", "Endemism",
                         "phytogeographicDomain")]
  dist_final <- merge(dist_final, Local_final, by = "id")
  dist_final <- unique(dist_final[,colnames(dist_final)])

  #Merge all information
  df_final1 <- merge(taxon, vernacular_final, by = "id", all = TRUE)
  df_final2 <- merge(df_final1, spProfile, by = "id", all = TRUE)
  df_final3 <- merge(df_final2, dist_final, by = "id", all = TRUE)

  #Create columns with name of the specie and accepted name
  df_final3$species <- NA
  #Ignore this ranks
  ignore_rank <- c("ORDEM", "FAMILIA", "GENERO", "CLASSE", "TRIBO",
                   "SUB_FAMILIA", "DIVISAO")
  df_final3$species[which(!(df_final3$taxonRank %in% ignore_rank))] <-
    gsub("^((\\w+\\W+){1}\\w+).*$","\\1",
         df_final3$scientificName[which(!(df_final3$taxonRank %in%
                                            ignore_rank))])

  #Accepted name when is synonymn
  df_final3$acceptedName <- NA
  df_final3$acceptedName[which(!(df_final3$taxonRank %in% ignore_rank))] <-
    gsub("^((\\w+\\W+){1}\\w+).*$","\\1",
         df_final3$acceptedNameUsage[which(!(df_final3$taxonRank %in%
                                               ignore_rank))])

  #Get Group and subgroup
  #Group
  df_final3$Group <- extract_between(str = df_final3$higherClassification,
                                     left = ";", right = ";")
  df_final3$Group <- translate_group(Group = df_final3$Group)
  #Subgroup - Only Bryophytes and Fungi
  df_final3$Subgroup <- NA
  df_final3$Subgroup[which(df_final3$Group ==
                             "Bryophytes")] <- extract_between(
    str = df_final3$higherClassification[which(
      df_final3$Group == "Bryophytes")],
    left = "Briofitas;",
    right = ";")
  df_final3$Subgroup[which(df_final3$Group == "Fungi")] <- extract_between(
    str = df_final3$higherClassification[which(df_final3$Group == "Fungi")],
    left = "Fungos;",
    right = ";")
  df_final3$Subgroup <- translate_subgroup(Subgroup = df_final3$Subgroup)

  #Order columns
  df_final <- df_final3[,c(c("id", "taxonID", "acceptedNameUsageID",
                             "parentNameUsageID", "originalNameUsageID",
                             "Group", "Subgroup",
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
                             "habitat", "vegetationType",
                             "Origin", "Endemism", "phytogeographicDomain",
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
  #Update distribution - They do not occur in Brazil
  sp_out_unique$vegetationType <- "Not_found_in_Brazil"
  sp_out_unique$Endemism <- "Not_found_in_Brazil"
  sp_out_unique$Origin <- "Not_found_in_Brazil"
  sp_out_unique$locationID <- "Not_found_in_Brazil"
  sp_out_unique$phytogeographicDomain <- "Not_found_in_Brazil"
  #Update taxonomic info
  sp_out_unique$species <- sp_out_unique$acceptedName
  sp_out_unique$scientificName <- sp_out_unique$acceptedNameUsage
  sp_out_unique$nomenclaturalStatus <- "NOME_CORRETO"
  sp_out_unique$taxonomicStatus <- "NOME_ACEITO"
  sp_out_unique$genus <- gsub( " .*$", "", sp_out_unique$species)
  sp_out_unique$specificEpithet <- gsub( ".* ", "", sp_out_unique$species)
  sp_out_unique$id <- sp_out_unique$acceptedNameUsageID
  sp_out_unique$taxonID <- sp_out_unique$acceptedNameUsageID
  #Join info
  df_join <- rbind(df_final, sp_out_unique)

  #Translate
  df_join$lifeForm <- translate_lifeform(lifeform = df_join$lifeForm)
  df_join$habitat <- translate_habitat(habitat = df_join$habitat)
  df_join$phytogeographicDomain <- translate_biome(biome =
                                                df_join$phytogeographicDomain)
  df_join$vegetationType <- translate_vegetation(vegetation =
                                                   df_join$vegetationType)
  df_join$Endemism <- translate_Endemism(Endemism = df_join$Endemism)
  df_join$Origin <- translate_Origin(Origin = df_join$Origin)
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
  df_join$vegetationType<- vapply(df_join$vegetationType,
                                  FUN.VALUE = character(1),
                                  function(x){
    paste(sort(unlist(strsplit(x, split = ","))),collapse = ";")
  }, USE.NAMES = FALSE)

  #Replace space by underline in Biome and vegetation
  df_join$phytogeographicDomain <- gsub(" ", "_", df_join$phytogeographicDomain)
  df_join$vegetationType <- gsub(" ", "_", df_join$vegetationType)
  df_join$Endemism <- gsub(" ", "_", df_join$Endemism)
  df_join$Origin <- gsub(" ", "_", df_join$Origin)

  #Rename columns
  colnames(df_join)[colnames(df_join) == "phytogeographicDomain"] <- "Biome"
  colnames(df_join)[colnames(df_join) == "locationID"] <- "States"

  #Save as RDS
  saveRDS(df_join,
          file = file.path(path_data, version_data,
                           "CompleteBrazilianFlora.rds"))

}

