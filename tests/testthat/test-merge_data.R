make_dwca_fixture <- function(path_data, version_data) {
  version_dir <- file.path(path_data, version_data)
  dir.create(version_dir, recursive = TRUE)

  taxon <- data.frame(
    id = c(33971L, 33972L),
    taxonID = c(33971L, 33972L),
    acceptedNameUsageID = c(33971L, 33972L),
    parentNameUsageID = c(NA_integer_, 33971L),
    originalNameUsageID = c(NA_integer_, NA_integer_),
    scientificName = c(
      "Araucaria angustifolia", "Araucaria angustifolia subsp. minor"
    ),
    acceptedNameUsage = c(
      "Araucaria angustifolia", "Araucaria angustifolia subsp. minor"
    ),
    parentNameUsage = c("Araucaria", "Araucaria angustifolia"),
    namePublishedIn = c("Example", "Example"),
    namePublishedInYear = c(1900L, 1901L),
    higherClassification = rep(
      "Plantae;Gimnospermas;Araucariaceae;", 2L
    ),
    kingdom = rep("Plantae", 2L),
    phylum = rep("Tracheophyta", 2L),
    class = rep("Pinopsida", 2L),
    order = rep("Pinales", 2L),
    family = rep("Araucariaceae", 2L),
    genus = rep("Araucaria", 2L),
    specificEpithet = rep("angustifolia", 2L),
    infraspecificEpithet = c(NA_character_, "minor"),
    taxonRank = c("ESPECIE", "SUB_ESPECIE"),
    scientificNameAuthorship = rep("Example", 2L),
    taxonomicStatus = rep("NOME_ACEITO", 2L),
    nomenclaturalStatus = rep("NOME_CORRETO", 2L),
    modified = rep("2026-10-01", 2L),
    bibliographicCitation = rep("Example citation", 2L),
    references = rep("https://example.org", 2L)
  )

  vernacular <- data.frame(
    id = c(33971L, 33971L, 33972L),
    vernacularName = c("pinheiro", "araucaria", "pinheiro menor")
  )

  profile <- data.frame(
    id = c(33971L, 33972L),
    lifeForm = c(
      paste0(
        '{"lifeForm":["\\u00c1rvore"],',
        '"habitat":["Terr\\u00edcola"],',
        '"vegetationType":["Floresta Ombr\\u00f3fila Mista"]}'
      ),
      paste0(
        '{"lifeForm":["Arbusto"],',
        '"habitat":["Terricola"],',
        '"vegetationType":["Campo de Altitude"]}'
      )
    ),
    habitat = c(NA_character_, NA_character_)
  )

  species_remarks <- paste0(
    '{"endemism":"N\\u00e3o endemica",',
    '"phytogeographicDomain":["Mata Atl\\u00e2ntica","Pampa"]}'
  )
  subspecies_remarks <- paste0(
    '{"endemism":"Endemica",',
    '"phytogeographicDomain":["Cerrado"]}'
  )
  distribution <- data.frame(
    id = c(33971L, 33971L, 33972L),
    locationID = c("BR-RJ", "BR-SC", "BR-PR"),
    countryCode = rep("BR", 3L),
    establishmentMeans = rep("Nativa", 3L),
    occurrenceRemarks = c(
      species_remarks, species_remarks, subspecies_remarks
    )
  )

  tables <- list(
    taxon.txt = taxon,
    vernacularname.txt = vernacular,
    speciesprofile.txt = profile,
    distribution.txt = distribution
  )
  for (filename in names(tables)) {
    utils::write.table(
      tables[[filename]], file = file.path(version_dir, filename),
      sep = "\t", na = "", quote = FALSE, row.names = FALSE
    )
  }
  invisible(version_dir)
}

test_that("merge_data processes local Darwin Core JSON tables", {
  path_data <- tempfile("florabr-dwca-")
  on.exit(unlink(path_data, recursive = TRUE), add = TRUE)
  version_dir <- make_dwca_fixture(path_data, "393.432")

  expect_true(all(file.exists(file.path(
    version_dir,
    c("taxon.txt", "distribution.txt", "speciesprofile.txt",
      "vernacularname.txt")
  ))))
  expect_invisible(merge_data(
    path_data, version_data = "393.432", verbose = FALSE
  ))

  merged <- data.table::fread(
    file.path(version_dir, "CompleteBrazilianFlora.gz"),
    data.table = FALSE
  )
  expect_equal(nrow(merged), 2L)

  species <- merged[merged$id == 33971L, , drop = FALSE]
  expect_equal(species$vernacularName, "pinheiro, araucaria")
  expect_equal(species$lifeForm, "Tree")
  expect_equal(species$habitat, "Terrestrial")
  expect_equal(species$vegetation, "Mixed_Ombrophyllous_Forest")
  expect_equal(species$endemism, "Non-endemic")
  expect_equal(species$origin, "Native")
  expect_equal(species$biome, "Atlantic_Forest;Pampa")
  expect_equal(species$states, "RJ;SC")

  subspecies <- merged[merged$id == 33972L, , drop = FALSE]
  expect_equal(subspecies$taxonRank, "Subspecies")
  expect_equal(subspecies$biome, "Cerrado")
  expect_equal(subspecies$states, "PR")
})

test_that("merge_data persists resolved species discrepancies", {
  path_data <- tempfile("florabr-solved-")
  on.exit(unlink(path_data, recursive = TRUE), add = TRUE)
  version_dir <- make_dwca_fixture(path_data, "393.432")

  expect_invisible(merge_data(
    path_data, version_data = "393.432",
    solve_discrepancy = TRUE, verbose = FALSE
  ))

  merged <- data.table::fread(
    file.path(version_dir, "CompleteBrazilianFlora.gz"),
    data.table = FALSE
  )
  expect_equal(nrow(merged), 2L)
  species <- merged[merged$id == 33971L, , drop = FALSE]
  expect_equal(species$states, "PR;RJ;SC")
  expect_equal(species$biome, "Atlantic_Forest;Cerrado;Pampa")
  expect_equal(species$lifeForm, "Shrub;Tree")
  expect_equal(
    species$vegetation,
    "High_Altitude_Grassland;Mixed_Ombrophyllous_Forest"
  )
  expect_equal(merged$states[merged$id == 33972L], "PR")
})

test_that("merge_data selects the latest local version and checks inputs", {
  path_data <- tempfile("florabr-versions-")
  on.exit(unlink(path_data, recursive = TRUE), add = TRUE)
  old_dir <- make_dwca_fixture(path_data, "393.9")
  new_dir <- make_dwca_fixture(path_data, "393.10")

  expect_invisible(merge_data(
    path_data, version_data = "latest", verbose = FALSE
  ))
  expect_true(file.exists(file.path(
    new_dir, "CompleteBrazilianFlora.gz"
  )))
  expect_false(file.exists(file.path(
    old_dir, "CompleteBrazilianFlora.gz"
  )))

  unlink(file.path(new_dir, "distribution.txt"))
  expect_error(
    merge_data(path_data, version_data = "393.10", verbose = FALSE),
    "Missing source files"
  )
})
