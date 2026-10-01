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
  # tolower
  origin <- tolower(origin)
  neworigin <- ifelse(
    origin == "", "Unknown",
    ifelse(origin == "nativa", "Native",
           ifelse(origin %in% c("ex\u00f3tica", "exotica"), "exotic",
                  ifelse(origin == "cultivada", "Cultivated",
                         ifelse(origin == "naturalizada", "Naturalized",
                                ifelse(origin == "nao ocorre no Brasil",
                                       "Not_found_in_brazil", NA))))))

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

ipt_latest_version <- function(base_url, ua) {
  tryCatch({
    info <- httr::HEAD(
      base_url,
      httr::user_agent(ua),
      httr::timeout(15)
    )
    httr::stop_for_status(info)

    disposition <- httr::headers(info)[["content-disposition"]]
    if (is.null(disposition)) {
      stop("The response has no Content-Disposition header.")
    }

    match <- regmatches(
      disposition,
      regexec("-v([0-9]+(?:\\.[0-9]+)*)\\.zip",
              disposition, perl = TRUE)
    )[[1]]

    if (length(match) < 2L) {
      stop("The archive filename does not contain a valid version.")
    }

    match[2]
  }, error = function(e) {
    stop(
      "Could not determine the latest Fauna do Brasil version from the IPT: ",
      conditionMessage(e),
      call. = FALSE
    )
  })
}

# Parse each distinct JSON remark only once.
parse_remark <- function(text) {
  result <- list(
    endemism = NA_character_,
    phytogeographicDomain = NA_character_
  )

  if (is.na(text) || !nzchar(text)) {
    return(result)
  }

  metadata <- tryCatch(
    jsonlite::fromJSON(text),
    error = function(e) NULL
  )

  if (!is.list(metadata)) {
    return(result)
  }

  if (length(metadata$endemism) > 0L &&
      !is.na(metadata$endemism[1L])) {
    result$endemism <- iconv(
      as.character(metadata$endemism[1L]),
      to = "ASCII//TRANSLIT"
    )
  }

  domains <- as.character(metadata$phytogeographicDomain)
  domains <- domains[!is.na(domains) & nzchar(domains)]

  if (length(domains) > 0L) {
    result$phytogeographicDomain <- paste(
      iconv(domains, to = "ASCII//TRANSLIT"),
      collapse = ","
    )
  }

  result
}

read_table <- function(target_dir, filename, encoding) {
  data.table::fread(
    file.path(target_dir, filename),
    sep = "\t",
    encoding = encoding,
    na.strings = "",
    check.names = TRUE,
    strip.white = FALSE
  )
}

# Parse each distinct JSON profile only once.
parse_profile <- function(text) {
  empty <- list(
    lifeForm = NA_character_,
    habitat = NA_character_,
    vegetation = NA_character_
  )

  if (is.na(text) || !nzchar(text)) {
    return(empty)
  }

  metadata <- tryCatch(
    jsonlite::fromJSON(text),
    error = function(e) NULL
  )

  if (!is.list(metadata)) {
    return(empty)
  }

  combine_values <- function(values) {
    values <- as.character(values)
    values <- values[!is.na(values) & nzchar(values)]

    if (length(values) == 0L) {
      return(NA_character_)
    }

    paste(
      iconv(values, to = "ASCII//TRANSLIT"),
      collapse = ","
    )
  }

  list(
    lifeForm = combine_values(metadata$lifeForm),
    habitat = combine_values(metadata$habitat),
    vegetation = combine_values(metadata$vegetationType)
  )
}

# fread() requires R.utils internally to read gz files
florabr_gzip_dependency <- function() {
  R.utils::gunzip
}
