#' Check if you have the latest version of Flora e Funga do Brasil data
#' available
#'
#' @description
#' This function checks if you have the latest version of the Flora e Funga do
#' Brasil data available in a specified directory.
#'
#' @param data_dir the directory where the data should be located.
#'
#' @return A message informing whether you have the latest version of Flora e
#' Funga do Brasil available in the data_dir
#' @usage check_version(data_dir)
#' @export
#'
#' @examples
#' \dontrun{
#' #Check if there is a version of Flora e Funga do Brasil data available in the
#' #current directory
#' check_version(data_dir = getwd())
#' }
#'
check_version <- function(data_dir) {
  if (missing(data_dir) || !is.character(data_dir) ||
      length(data_dir) != 1L || is.na(data_dir) ||
      !dir.exists(data_dir)) {
    stop("data_dir must be an existing directory.", call. = FALSE)
  }

  # Find version folders containing a merged dataset.
  directories <- list.dirs(
    path = data_dir, recursive = FALSE, full.names = FALSE
  )
  local_versions <- directories[
    grepl("^[0-9]+(\\.[0-9]+)+$", directories) &
      file.exists(file.path(
        data_dir, directories, "CompleteBrazilianFlora.gz"
      ))
  ]

  base_url <- paste0(
    "https://ipt.jbrj.gov.br/jbrj/archive.do",
    "?r=lista_especies_flora_brasil"
  )
  ua <- paste(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
    "AppleWebKit/537.36 Chrome/120.0.0.0 Safari/537.36"
  )

  latest_version <- tryCatch(
    ipt_latest_version(base_url, ua),
    error = function(e) {
      message("Could not check the latest version on the IPT: ",
              conditionMessage(e))
      NULL
    }
  )

  if (length(local_versions) == 0L) {
    message("No local version of Flora e Funga do Brasil was found.")
  } else {
    message("Local versions: ", paste(local_versions, collapse = ", "))
  }

  if (is.null(latest_version)) {
    message("The latest online version could not be verified.")
  } else if (latest_version %in% local_versions) {
    message("You have the latest version: ", latest_version)
  } else {
    message(
      "The latest version is ", latest_version,
      " and is not available in this directory."
    )
  }

  invisible(NULL)
}
