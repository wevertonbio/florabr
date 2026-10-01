test_that("load_florabr loads short and complete datasets", {
  my_dir <- tempfile("florabr-load-")
  on.exit(unlink(my_dir, recursive = TRUE), add = TRUE)

  short_columns <- c(
    "species", "scientificName", "acceptedName", "kingdom",
    "group", "subgroup", "phylum", "class", "order", "family",
    "genus", "lifeForm", "habitat", "biome", "states",
    "vegetation", "origin", "endemism", "taxonomicStatus",
    "nomenclaturalStatus", "vernacularName", "taxonRank", "id"
  )

  # Provide every column selected by load_florabr(type = "short").
  example_data <- as.data.frame(
    setNames(
      rep(list(c("value_1", "value_2")), length(short_columns)),
      short_columns
    ),
    stringsAsFactors = FALSE
  )
  example_data$id <- c(1L, 2L)
  example_data$species <- c(
    "Araucaria angustifolia",
    "Abatia americana"
  )
  example_data$sourceNote <- c("first record", "second record")

  older_dir <- file.path(my_dir, "393.431")
  latest_dir <- file.path(my_dir, "393.432")
  dir.create(older_dir, recursive = TRUE)
  dir.create(latest_dir, recursive = TRUE)

  older_data <- example_data
  older_data$species[1] <- "Older version"

  data.table::fwrite(
    older_data,
    file.path(older_dir, "CompleteBrazilianFlora.gz"),
    compress = "gzip"
  )
  data.table::fwrite(
    example_data,
    file.path(latest_dir, "CompleteBrazilianFlora.gz"),
    compress = "gzip"
  )

  short <- load_florabr(
    data_dir = my_dir,
    data_version = "Latest_available",
    type = "short",
    verbose = FALSE
  )

  expect_s3_class(short, "data.frame")
  expect_identical(names(short), short_columns)
  expect_identical(short$species, example_data$species)
  expect_false("sourceNote" %in% names(short))

  complete <- load_florabr(
    data_dir = my_dir,
    data_version = "393.432",
    type = "complete",
    verbose = FALSE
  )

  expect_s3_class(complete, "data.frame")
  expect_true("sourceNote" %in% names(complete))
  expect_identical(complete$sourceNote, example_data$sourceNote)

  expect_error(
    load_florabr(
      data_dir = my_dir,
      data_version = "1",
      type = "complete"
    )
  )
  expect_error(
    load_florabr(
      data_dir = my_dir,
      data_version = "393.432",
      type = "anytype"
    )
  )
})

test_that("load_florabr rejects invalid arguments and missing data", {
  empty_dir <- tempfile("florabr-empty-")
  dir.create(empty_dir)
  on.exit(unlink(empty_dir, recursive = TRUE), add = TRUE)

  expect_error(load_florabr())
  expect_error(load_florabr(data_dir = TRUE))
  expect_error(
    load_florabr(
      data_dir = empty_dir,
      data_version = TRUE
    )
  )
  expect_error(
    load_florabr(
      data_dir = empty_dir,
      data_version = "Latest_available",
      type = "short"
    )
  )
})
