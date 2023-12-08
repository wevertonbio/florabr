#' Check if you have the latest version of Brazilian Flora data available
#'
#' @description
#' This function checks if you have the latest version of the Brazilian Flora
#' data available in a specified directory.
#'
#' @param data_dir the directory where the data should be located.
#'
#' @return A message informing whether you have the latest version of Brazilian
#' Flora data available in the data_dir
#' @usage check_version(data_dir)
#' @export
#'
#' @importFrom httr GET
#' @importFrom XML htmlParse xpathSApply xmlGetAttr
#'
#' @examples
#' #Check if there is a version of Brazilian Flora data available in the current
#' #directory
#' check_version(data_dir = getwd())

check_version <- function(data_dir) {
  #Set folder
  if (missing(data_dir)) {
    stop("Argument data_dir is not defined")
  }

  if (!is.character(data_dir)) {
    stop(paste0("Argument data_dir must be a character, not ", class(data_dir)))
  } else {
    path_data <- data_dir
  }

  #Search for directories
  all_dirs <- list.dirs(path = path_data, recursive = FALSE,
                        full.names = FALSE)
  dir_versions <- subset(all_dirs, grepl("393", all_dirs)) #Actual version
  #Get highest version
  if(length(dir_versions) > 0) {
  high_version <- max(as.numeric(gsub("393.", "", dir_versions)))
  current_version <- paste0("393.", high_version) } else {
    current_version <- 0
  }

  #Get link of latest version
  response <- httr::GET(
    "https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil")
  parse <- XML::htmlParse(response)
  links <- unlist(XML::xpathSApply(parse, path = "//a", XML::xmlGetAttr,"href"))
  download_pattern <- "https://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil&v="
  link_download <- subset(links, grepl(download_pattern, links,
                                       fixed = TRUE))

  #Get version
  latest_version <- gsub(".*lista_especies_flora_brasil&v=([0-9.]+).*", "\\1",
                       link_download)

  #Check if you have the latest version
  is_latest <- current_version == latest_version

  #Check if final data exists in the folder
  file_exist <- file.exists(file.path(path_data, dir_versions,
                                      "CompleteBrazilianFlora.rds"))

  #Get only dir_versions with merged data
  dir_versions <- dir_versions[file_exist]

  #Check how many versions exists
  many_versions <- length(dir_versions) > 1

  #Print messages

  if(length(dir_versions) == 0) {
    message(
    "You do not have any version of Flora do Brazil 2020 in this directory.
    The latest version is ", latest_version, ". Please, change the directory or
    run the function get_florabr() to download the latest version of Brazilian
    Flora 2020.", "\n")
  }

  if(isTRUE(is_latest) & isFALSE(many_versions)) {
    message(paste("You have the latest version of Brazilian Flora 2020 Data -
              Version", latest_version, "\n"))
  }

  if(isTRUE(is_latest) & isTRUE(many_versions)) {
    message(paste("You have the following versions of Brazilian Flora:\n",
              paste(dir_versions, collapse = "\n"),
              "\n It includes the latest version: ",
              latest_version, "\n"))
  }

  if(isFALSE(is_latest) & isTRUE(many_versions)) {
    message(paste0("You have the following versions of Brazilian Flora:\n",
              paste0(dir_versions, collapse = "\n"),
              "\nHowever, it does not include the latest version: ",
              latest_version,
    "\nIf you want to download the latest version, run the function
    get_florabr() again"))
  }
}
