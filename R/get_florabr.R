#' Download the latest version of Flora e Funga do Brasil database
#'
#' @description
#' This function downloads the latest or an older version of Flora e Funga do
#' Brasil database, merges the information into a single data.frame, and saves
#' this data.frame in the specified directory.
#'
#'
#' @param output_dir (character) a directory to save the data downloaded from
#' Flora e Funga do Brasil.
#' @param data_version (character) Version of the Flora e Funga do Brasil
#' database to download. Use "latest" to get the most recent version, updated weekly.
#' Alternatively, specify an older version (e.g., data_version = "393.319").
#' Default value is "latest".
#' @param solve_discrepancy Resolve discrepancies between species and
#' subspecies/varieties  information. When set to TRUE, species
#' information is updated based on unique data from varieties and subspecies.
#' For example, if a subspecies occurs in a certain biome, it implies that the
#' species also occurs in that biome. Default = FALSE.
#' @param overwrite (logical) If TRUE, data is overwritten. Default = TRUE.
#' @param verbose (logical) Whether to display messages during function
#' execution. Set to TRUE to enable display, or FALSE to run silently.
#' Default = TRUE.
#' @param remove_files (logical) Whether to remove the downloaded files used in
#' building the final dataset. Default is TRUE.
#' @param get_fixed_version (logical) If TRUE, download the fixed and
#' already-merged version 393.432 from Zenodo instead of downloading and
#' processing an IPT archive. Default is FALSE.
#'
#' @returns
#' The function downloads the latest version of the Flora e Funga do Brasil
#' database from the official source. It then merges the information into a
#' single data.frame, containing details on species, taxonomy, occurrence,
#' and other relevant data.
#' The merged data.frame is then saved as a file in the specified output
#' directory. The data is saved in a format that allows easy loading using the
#' \code{\link{load_florabr}} function for further analysis in R.
#' @usage get_florabr(output_dir, data_version = "latest",
#'                  solve_discrepancy = FALSE, overwrite = TRUE,
#'                  verbose = TRUE, remove_files = TRUE,
#'                  get_fixed_version = FALSE)
#' @export
#'
#' @importFrom httr GET user_agent timeout write_disk stop_for_status
#' @importFrom utils unzip
#' @importFrom tools md5sum
#' @references
#' Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#' @examples
#' \dontrun{
#' #Creating a folder in a temporary directory
#' #Replace 'file.path(tempdir(), "florabr")' by a path folder to be create in
#' #your computer
#' my_dir <- file.path(file.path(tempdir(), "florabr"))
#' dir.create(my_dir)
#' #Download, merge and save data
#' get_florabr(output_dir = my_dir, data_version = "latest",
#'             solve_discrepancy = FALSE, overwrite = TRUE, verbose = TRUE)
#' }
get_florabr <- function(output_dir, data_version = "latest",
                        solve_discrepancy = FALSE,
                        overwrite = TRUE,
                        verbose = TRUE,
                        remove_files = TRUE,
                        get_fixed_version = FALSE) {
  if (missing(output_dir) || !is.character(output_dir) ||
      length(output_dir) != 1L || is.na(output_dir) ||
      !nzchar(output_dir)) {
    stop("output_dir must be a single directory path.", call. = FALSE)
  }

  if (!dir.exists(output_dir)) {
    stop("output_dir must be an existing directory: ", output_dir,
         call. = FALSE)
  }

  if (!is.character(data_version) ||
      length(data_version) != 1L ||
      is.na(data_version) ||
      !grepl("^(latest|[0-9]+(\\.[0-9]+)+)$", data_version)) {
    stop(
      "data_version must be 'latest' or a version such as '393.432'.",
      call. = FALSE
    )
  }

  check_flag <- function(value, name) {
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop(name, " must be TRUE or FALSE.", call. = FALSE)
    }
  }

  check_flag(solve_discrepancy, "solve_discrepancy")
  check_flag(overwrite, "overwrite")
  check_flag(verbose, "verbose")
  check_flag(remove_files, "remove_files")
  check_flag(get_fixed_version, "get_fixed_version")

  path_data <- output_dir

  if (verbose) {
    message("Data will be saved in ", path_data, "\n")
  }

  # The Zenodo file is already merged; it is not a Darwin Core ZIP.
  if (get_fixed_version) {
    fixed_version <- "393.432"
    fixed_url <- paste0(
      "https://zenodo.org/records/23084491/files/",
      "CompleteBrazilianFlora.gz?download=1"
    )
    fixed_md5 <- "f73751178516ebcf6ad78a5e0ac3fa17"

    if (!data_version %in% c("latest", fixed_version)) {
      stop(
        "get_fixed_version = TRUE provides only version ",
        fixed_version, "; requested version: ", data_version,
        call. = FALSE
      )
    }

    if (solve_discrepancy) {
      stop(
        "solve_discrepancy cannot be changed when downloading ",
        "the already-merged fixed version.",
        call. = FALSE
      )
    }

    version_dir <- file.path(path_data, fixed_version)
    output_file <- file.path(
      version_dir, "CompleteBrazilianFlora.gz"
    )

    if (file.exists(output_file) && !overwrite) {
      stop(
        "The file already exists and overwrite = FALSE: ",
        output_file,
        call. = FALSE
      )
    }

    # A failed transfer must not replace an existing dataset.
    temp_file <- tempfile(
      pattern = "florabr-zenodo-",
      tmpdir = path_data,
      fileext = ".gz"
    )
    on.exit(unlink(temp_file), add = TRUE)

    if (verbose) {
      message("Downloading fixed version ", fixed_version,
              " from Zenodo...")
    }

    tryCatch(
      {
        response <- httr::GET(
          fixed_url,
          httr::timeout(180),
          httr::write_disk(temp_file, overwrite = TRUE)
        )
        httr::stop_for_status(response)
      },
      error = function(e) {
        stop(
          "Could not download Flora e Funga do Brasil version ",
          fixed_version, " from Zenodo: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

    # Verify the file against the checksum published by Zenodo.
    actual_md5 <- unname(tools::md5sum(temp_file))
    if (is.na(actual_md5) || !identical(actual_md5, fixed_md5)) {
      stop(
        "The downloaded Zenodo file failed checksum verification.",
        call. = FALSE
      )
    }

    if (!dir.exists(version_dir) &&
        !dir.create(version_dir, recursive = TRUE)) {
      stop("Could not create directory: ", version_dir,
           call. = FALSE)
    }

    if (!file.copy(temp_file, output_file, overwrite = overwrite)) {
      stop("Could not save the downloaded file: ", output_file,
           call. = FALSE)
    }

    if (verbose) {
      message("Fixed version saved in ", output_file)
    }

    # remove_files has no effect here: there are no raw files to remove.
    return(invisible(output_file))
  }

  # From here onward, use the original IPT Darwin Core workflow.
  base_url <- paste0(
    "https://ipt.jbrj.gov.br/jbrj/archive.do",
    "?r=lista_especies_flora_brasil"
  )
  ua <- paste(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
    "AppleWebKit/537.36 Chrome/120.0.0.0 Safari/537.36"
  )

  if (data_version == "latest") {
    version_data <- ipt_latest_version(base_url, ua)
  } else {
    version_data <- data_version
  }

  # Pin the version between the HEAD and GET requests.
  link_download <- paste0(base_url, "&v=", version_data)

  if (verbose) {
    message("Downloading version: ", version_data, "\n")
  }

  zip_path <- file.path(path_data, paste0(version_data, ".zip"))

  if (file.exists(zip_path) && !overwrite) {
    stop(
      "The ZIP file already exists and overwrite = FALSE: ",
      zip_path,
      call. = FALSE
    )
  }

  # Download to a temporary file so an HTTP error cannot replace a ZIP.
  temp_zip <- tempfile(
    pattern = "florabr-", tmpdir = path_data, fileext = ".zip"
  )
  on.exit(unlink(temp_zip), add = TRUE)

  tryCatch(
    {
      response <- httr::GET(
        link_download,
        httr::user_agent(ua),
        httr::timeout(120),
        httr::write_disk(temp_zip, overwrite = TRUE)
      )
      httr::stop_for_status(response)
    },
    error = function(e) {
      stop(
        "Could not download Flora e Funga do Brasil version ",
        version_data, " from the IPT: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  archive <- tryCatch(
    utils::unzip(temp_zip, list = TRUE),
    warning = function(w) {
      stop(
        "The IPT response is not a valid ZIP file: ",
        conditionMessage(w),
        call. = FALSE
      )
    },
    error = function(e) {
      stop(
        "Could not read the downloaded ZIP file: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  required_files <- c(
    "taxon.txt",
    "vernacularname.txt",
    "speciesprofile.txt",
    "distribution.txt"
  )

  if (!all(required_files %in% archive$Name)) {
    stop(
      "The IPT archive does not contain the expected data files.",
      call. = FALSE
    )
  }

  if (!file.copy(temp_zip, zip_path, overwrite = overwrite)) {
    stop("Could not save the downloaded ZIP file: ", zip_path,
         call. = FALSE)
  }

  version_dir <- file.path(path_data, version_data)

  utils::unzip(zipfile = zip_path, exdir = version_dir)

  if (!all(file.exists(file.path(version_dir, required_files)))) {
    stop(
      "Could not extract all required data files into ",
      version_dir,
      call. = FALSE
    )
  }

  if (verbose) {
    message("Merging data. Please wait a moment...\n")
  }

  merge_data(
    path_data = path_data,
    version_data = version_data,
    solve_discrepancy = solve_discrepancy,
    verbose = verbose
  )

  output_file <- file.path(
    version_dir, "CompleteBrazilianFlora.gz"
  )

  if (!file.exists(output_file)) {
    stop(
      "merge_data() did not create the expected file: ",
      output_file,
      call. = FALSE
    )
  }

  if (remove_files) {
    # Remove only direct files listed in this downloaded archive.
    # Do not delete unrelated files already present in version_dir.
    archive_files <- archive$Name
    archive_files <- archive_files[
      nzchar(archive_files) &
        basename(archive_files) == archive_files &
        archive_files != "CompleteBrazilianFlora.gz"
    ]

    unlink(file.path(version_dir, archive_files))
    unlink(zip_path)
  }

  if (verbose) {
    message(
      "Data downloaded and merged successfully. ",
      "Final data saved in ", output_file
    )
  }

  invisible(output_file)
}
