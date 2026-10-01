#' Merge Flora e Funga do Brasil data
#'
#' @description
#' Reads the Darwin Core Archive tables, merges taxonomic and distribution
#' information, and saves a merged dataset as CompleteBrazilianFlora.gz.
#'
#' @param path_data Directory containing the extracted version folders.
#' @param version_data Version to merge, or "latest" for the newest locally
#'   available version.
#' @param solve_discrepancy Whether to resolve discrepancies between species
#'   and infraspecific taxa. Default is FALSE.
#' @param encoding Encoding passed to data.table::fread(). Default is "UTF-8".
#' @param verbose Whether to display progress messages. Default is TRUE.
#'
#' @return Invisibly returns NULL. Saves CompleteBrazilianFlora.gz in the
#'   selected version folder.
#'
#' @importFrom data.table set data.table rbindlist setnames fwrite
#' @importFrom stats na.omit
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' merge_data(
#'   path_data = "data",
#'   version_data = "393.431",
#'   solve_discrepancy = FALSE
#' )
#' }
merge_data <- function(path_data, version_data = "latest",
                       solve_discrepancy = FALSE,
                       encoding = "UTF-8", verbose = TRUE) {
  if (!is.character(path_data) || length(path_data) != 1L ||
      is.na(path_data) || !dir.exists(path_data)) {
    stop("path_data must be an existing directory.", call. = FALSE)
  }

  if (!is.character(version_data) || length(version_data) != 1L ||
      is.na(version_data)) {
    stop("version_data must be a single character value.", call. = FALSE)
  }

  if (!is.logical(solve_discrepancy) ||
      length(solve_discrepancy) != 1L || is.na(solve_discrepancy)) {
    stop("solve_discrepancy must be TRUE or FALSE.", call. = FALSE)
  }

  if (!is.logical(verbose) || length(verbose) != 1L || is.na(verbose)) {
    stop("verbose must be TRUE or FALSE.", call. = FALSE)
  }

  required_files <- c(
    "taxon.txt",
    "vernacularname.txt",
    "speciesprofile.txt",
    "distribution.txt"
  )

  if (version_data == "latest") {
    directories <- list.dirs(
      path = path_data, recursive = FALSE, full.names = FALSE
    )
    directories <- directories[
      grepl("^[0-9]+(\\.[0-9]+)+$", directories)
    ]

    # Only consider versions whose source tables are still available.
    available <- vapply(
      directories,
      function(version) {
        all(file.exists(file.path(
          path_data, version, required_files
        )))
      },
      logical(1)
    )
    directories <- directories[available]

    if (length(directories) == 0L) {
      stop(
        "No local version with all required source files was found.",
        call. = FALSE
      )
    }

    version_data <- directories[
      order(package_version(directories), decreasing = TRUE)
    ][1L]
  }

  target_dir <- file.path(path_data, version_data)
  missing_files <- required_files[
    !file.exists(file.path(target_dir, required_files))
  ]

  if (length(missing_files) > 0L) {
    stop(
      "Missing source files in ", target_dir, ": ",
      paste(missing_files, collapse = ", "),
      call. = FALSE
    )
  }

  if (verbose) {
    message("Merging Flora e Funga do Brasil version ", version_data)
    message("Reading source tables...")
  }

  taxon <- read_table(target_dir, "taxon.txt", encoding)
  vernacular <- read_table(target_dir, "vernacularname.txt", encoding)
  profile <- read_table(target_dir, "speciesprofile.txt", encoding)
  dist <- read_table(target_dir, "distribution.txt", encoding)

  # Preserve the text transformations performed by the original function.
  data.table::set(
    taxon, j = "higherClassification",
    value = iconv(taxon[["higherClassification"]], to = "ASCII//TRANSLIT")
  )
  data.table::set(
    vernacular, j = "vernacularName",
    value = iconv(vernacular[["vernacularName"]], to = "ASCII//TRANSLIT")
  )

  if (verbose) message("Aggregating vernacular names...")

  vernacular_final <- vernacular[
    !is.na(vernacular[["id"]]),
    list(
      vernacularName = paste(get("vernacularName"), collapse = ", ")
    ),
    by = "id"
  ]

  # The original code extracts all three attributes from lifeForm, including
  # habitat and vegetation. Preserve that behavior.
  profile_text <- profile[["lifeForm"]]
  distinct_profiles <- unique(profile_text)
  parsed_profiles <- lapply(distinct_profiles, parse_profile)
  profile_index <- match(profile_text, distinct_profiles)

  profile_final <- data.table::data.table(
    id = profile[["id"]],
    lifeForm = vapply(
      parsed_profiles, function(x) x$lifeForm, character(1)
    )[profile_index],
    habitat = vapply(
      parsed_profiles, function(x) x$habitat, character(1)
    )[profile_index],
    vegetation = vapply(
      parsed_profiles, function(x) x$vegetation, character(1)
    )[profile_index]
  )

  # Process distribution records
  if (verbose) message("Processing distribution records...")

  remarks <- dist[["occurrenceRemarks"]]
  distinct_remarks <- unique(remarks)
  parsed <- lapply(distinct_remarks, parse_remark)
  remark_index <- match(remarks, distinct_remarks)

  data.table::set(
    dist, j = "origin",
    value = dist[["establishmentMeans"]]
  )
  data.table::set(
    dist, j = "endemism",
    value = vapply(
      parsed, function(x) x$endemism, character(1)
    )[remark_index]
  )
  data.table::set(
    dist, j = "phytogeographicDomain",
    value = vapply(
      parsed, function(x) x$phytogeographicDomain, character(1)
    )[remark_index]
  )

  data.table::set(
    dist, j = "locationID",
    value = gsub(".*-", "", dist[["locationID"]])
  )

  # Aggregate locations, but retain the other distribution combinations.
  # Collapsing all distribution rows to one row per id would change the data.
  local_final <- dist[
    !is.na(dist[["id"]]),
    list(
      locationID = paste(get("locationID"), collapse = ";")
    ),
    by = "id"
  ]

  dist_columns <- c(
    "id", "countryCode", "origin",
    "endemism", "phytogeographicDomain"
  )
  dist_final <- merge(
    dist[, dist_columns, with = FALSE],
    local_final,
    by = "id",
    allow.cartesian = TRUE
  )
  dist_final <- unique(dist_final)

  if (verbose) message("Merging taxonomic tables...")

  # These are full outer joins, matching merge(..., all = TRUE) in the
  # original function.
  df <- merge(
    taxon, vernacular_final,
    by = "id", all = TRUE, allow.cartesian = TRUE
  )
  df <- merge(
    df, profile_final,
    by = "id", all = TRUE, allow.cartesian = TRUE
  )
  df <- merge(
    df, dist_final,
    by = "id", all = TRUE, allow.cartesian = TRUE
  )

  if (verbose) message("Processing names and classifications...")

  data.table::set(
    df, j = "species", value = rep(NA_character_, nrow(df))
  )

  ranks <- df[["taxonRank"]]
  genus <- df[["genus"]]
  epithet <- df[["specificEpithet"]]
  infra <- df[["infraspecificEpithet"]]

  species_rows <- which(ranks == "ESPECIE")
  variety_rows <- which(ranks == "VARIEDADE")
  subspecies_rows <- which(ranks == "SUB_ESPECIE")

  data.table::set(
    df, i = species_rows, j = "species",
    value = paste(genus[species_rows], epithet[species_rows])
  )
  data.table::set(
    df, i = variety_rows, j = "species",
    value = paste(
      genus[variety_rows], epithet[variety_rows],
      "var.", infra[variety_rows]
    )
  )
  data.table::set(
    df, i = subspecies_rows, j = "species",
    value = paste(
      genus[subspecies_rows], epithet[subspecies_rows],
      "subsp.", infra[subspecies_rows]
    )
  )

  ignore_rank <- setdiff(
    unique(stats::na.omit(ranks)),
    c("ESPECIE", "VARIEDADE", "SUB_ESPECIE")
  )
  accepted_rows <- which(!(ranks %in% ignore_rank))

  data.table::set(
    df, j = "acceptedName", value = rep(NA_character_, nrow(df))
  )
  if (length(accepted_rows) > 0L) {
    data.table::set(
      df, i = accepted_rows, j = "acceptedName",
      value = get_binomial(
        species_names = as.character(
          df[["acceptedNameUsage"]][accepted_rows]
        )
      )
    )
  }

  group <- translate_group(
    extract_between(
      df[["higherClassification"]], left = ";", right = ";"
    )
  )
  data.table::set(df, j = "group", value = group)

  subgroup <- rep(NA_character_, nrow(df))
  bryophytes <- which(group == "Bryophytes")
  fungi <- which(group == "Fungi")

  subgroup[bryophytes] <- extract_between(
    df[["higherClassification"]][bryophytes],
    left = "Briofitas;", right = ";"
  )
  subgroup[fungi] <- extract_between(
    df[["higherClassification"]][fungi],
    left = "Fungos;", right = ";"
  )
  data.table::set(
    df, j = "subgroup",
    value = translate_subgroup(subgroup)
  )

  selected_columns <- c(
    "id", "taxonID", "acceptedNameUsageID", "parentNameUsageID",
    "originalNameUsageID", "group", "subgroup", "species",
    "acceptedName", "scientificName", "acceptedNameUsage",
    "parentNameUsage", "namePublishedIn", "namePublishedInYear",
    "higherClassification", "kingdom", "phylum", "class", "order",
    "family", "genus", "specificEpithet", "infraspecificEpithet",
    "taxonRank", "scientificNameAuthorship", "taxonomicStatus",
    "nomenclaturalStatus", "vernacularName", "lifeForm", "habitat",
    "vegetation", "origin", "endemism", "phytogeographicDomain",
    "locationID", "countryCode", "modified",
    "bibliographicCitation", "references"
  )
  df <- df[, selected_columns, with = FALSE]

  # Add accepted names that do not occur in the species column.
  absent_names <- setdiff(
    unique(df[["acceptedName"]]),
    unique(df[["species"]])
  )
  additional <- df[df[["acceptedName"]] %in% absent_names]
  additional <- additional[
    !duplicated(additional[["acceptedName"]])
  ]

  if (nrow(additional) > 0L) {
    for (column in c(
      "vegetation", "endemism", "origin", "locationID",
      "phytogeographicDomain"
    )) {
      data.table::set(
        additional, j = column, value = "Not_found_in_brazil"
      )
    }

    new_species <- get_binomial(
      as.character(additional[["acceptedName"]]),
      include_variety = FALSE,
      include_subspecies = FALSE
    )
    data.table::set(
      additional, j = "species", value = new_species
    )
    data.table::set(
      additional, j = "scientificName",
      value = additional[["acceptedNameUsage"]]
    )
    data.table::set(
      additional, j = "nomenclaturalStatus",
      value = "NOME_CORRETO"
    )
    data.table::set(
      additional, j = "taxonomicStatus",
      value = "NOME_ACEITO"
    )
    data.table::set(
      additional, j = "genus",
      value = gsub(" .*$", "", new_species)
    )
    data.table::set(
      additional, j = "specificEpithet",
      value = gsub(".* ", "", new_species)
    )
    data.table::set(
      additional, j = "id",
      value = additional[["acceptedNameUsageID"]]
    )
    data.table::set(
      additional, j = "taxonID",
      value = additional[["acceptedNameUsageID"]]
    )
    data.table::set(
      additional, j = "taxonRank", value = "Species"
    )

    df <- data.table::rbindlist(
      list(df, additional), use.names = TRUE
    )
  }

  if (verbose) message("Translating and formatting attributes...")

  data.table::set(
    df, j = "lifeForm",
    value = translate_lifeform(df[["lifeForm"]])
  )
  data.table::set(
    df, j = "habitat",
    value = translate_habitat(df[["habitat"]])
  )
  data.table::set(
    df, j = "phytogeographicDomain",
    value = translate_biome(df[["phytogeographicDomain"]])
  )
  data.table::set(
    df, j = "vegetation",
    value = translate_vegetation(df[["vegetation"]])
  )
  data.table::set(
    df, j = "endemism",
    value = translate_endemism(df[["endemism"]])
  )
  data.table::set(
    df, j = "origin",
    value = translate_origin(df[["origin"]])
  )
  data.table::set(
    df, j = "taxonomicStatus",
    value = translate_taxonomicStatus(df[["taxonomicStatus"]])
  )
  data.table::set(
    df, j = "nomenclaturalStatus",
    value = translate_nomenclaturalStatus(
      df[["nomenclaturalStatus"]]
    )
  )
  data.table::set(
    df, j = "taxonRank",
    value = translate_taxonRank(df[["taxonRank"]])
  )

  # Split each complete column once instead of calling strsplit()
  # separately for every row.
  collapse_sorted <- function(values, separator) {
    parts <- strsplit(values, separator, fixed = TRUE)
    vapply(
      parts,
      function(x) paste(sort(x), collapse = ";"),
      character(1),
      USE.NAMES = FALSE
    )
  }

  for (column in c(
    "lifeForm", "habitat", "phytogeographicDomain",
    "locationID", "vegetation"
  )) {
    separator <- if (column == "locationID") ";" else ","
    data.table::set(
      df, j = column,
      value = collapse_sorted(df[[column]], separator)
    )
  }

  data.table::set(
    df, j = "phytogeographicDomain",
    value = gsub(" ", "_", df[["phytogeographicDomain"]])
  )
  data.table::set(
    df, j = "vegetation",
    value = gsub(" ", "_", df[["vegetation"]])
  )
  data.table::set(
    df, j = "endemism",
    value = gsub(" ", "_", df[["endemism"]])
  )
  data.table::set(
    df, j = "origin",
    value = gsub(" ", "_", df[["origin"]])
  )

  data.table::setnames(
    df,
    c("phytogeographicDomain", "locationID"),
    c("biome", "states")
  )

  # These functions expect data.frame behavior, not data.table's [ method.
  result <- as.data.frame(df)

  if (solve_discrepancy) {
    result <- solve_discrepancies(result)
    result <- fill_NA(result)
  } else {
    result <- fill_NA(result)
    attr(result, "solve_discrepancies") <- FALSE
  }

  if (verbose) message("Saving final dataset...")

  data.table::fwrite(
    df, file = file.path(target_dir,
                               "CompleteBrazilianFlora.gz"),
    row.names = FALSE, compress = "gzip")

  if (verbose) message("Done!")
  invisible(NULL)
}
