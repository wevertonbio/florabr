#' Download the latest version of Brazilian Flora 2020 database
#'
#' @description
#' This function downloads the latest or an older version of Brazilian Flora
#' 2020 database, merges the information into a single data.frame, and saves
#' this data.frame in the specified directory.
#'
#'
#' @param output_dir (character) a directory to save the data downloaded from
#' Brazilian Flora 2020
#' @param data_version (character) Version of the Brazilian Flora database to
#' download. Use "latest" to get the most recent version, updated weekly.
#' Alternatively, specify an older version (e.g., data_version = "393.319").
#' Default value is "latest".
#' @param overwrite (logical) If TRUE, data is overwritten. Default = TRUE.
#' @param verbose (logical) Whether to display messages during function
#' execution. Set to TRUE to enable display, or FALSE to run silently.
#' Default = TRUE.
#'
#' @returns
#' The function downloads the latest version of the Brazilian Flora 2020
#' database from the official source. It then merges the information into a
#' single data.frame, containing details on species, taxonomy, occurrence,
#' and other relevant data.
#' The merged data.frame is then saved as a file in the specified output
#' directory. The data is saved in a format that allows easy loading using the
#' \code{\link{load_florabr}} function for further analysis in R.
#' @usage get_florabr(output_dir, data_version = "latest", overwrite = TRUE,
#'                    verbose = TRUE)
#' @export
#'
#' @importFrom httr GET write_disk
#' @importFrom XML htmlParse xpathSApply xmlGetAttr
#' @importFrom utils unzip
#' @importFrom utils read.csv
#' @references
#' Brazilian Flora 2020. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#' @examples
#' \donttest{
#' #Creating a folder in a temporary directory
#' #Replace 'file.path(tempdir(), "florabr")' by a path folder to be create in
#' #your computer
#' my_dir <- file.path(file.path(tempdir(), "florabr"))
#' dir.create(my_dir)
#' #Download, merge and save data
#' get_florabr(output_dir = my_dir, data_version = "latest", overwrite = TRUE,
#'             verbose = TRUE)
#' }
get_florabr <- function(output_dir, data_version = "latest", overwrite = TRUE,
                        verbose = TRUE) {
  #Set folder
  if(is.null(output_dir)) {
    stop(paste("Argument output_dir is not defined, this is necessary for",
         "\n downloading and saving data"))
  }
  if (!is.character(output_dir)) {
    stop(paste0("Argument output_dir must be a character, not ",
                class(output_dir)))
  } else {
    path_data <- output_dir
  }

  if (!is.character(data_version)) {
    stop(paste0("Argument data_version must be a character, not ",
                class(data_version)))
  }

  if (!is.logical(overwrite)) {
    stop(paste0("Argument overwrite must be logical, not ",
                class(overwrite)))
  }


  #Print message
  if(verbose) {
  message("Data will be saved in", path_data, "\n") }


  if(data_version != "latest") {
  link_download <- paste0(
    "https://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil&v=",
                          data_version)
  }


  if(data_version == "latest") {
  #Get link of latest version
  response <- httr::GET(
    "https://ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil")
  parse <- XML::htmlParse(response)
  links <- unlist(XML::xpathSApply(parse, path = "//a", XML::xmlGetAttr,
                                   "href"))
  download_pattern <- "https://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil&v="
  link_download <- subset(links, grepl(download_pattern, links, fixed = TRUE))
  }

  #Get version
  version_data <- gsub(".*lista_especies_flora_brasil&v=([0-9.]+).*", "\\1",
                       link_download)

  #Print message
  if(!is.null(version_data) & verbose) {
      message("Downloading version:", version_data, "\n")


  #Download data
  httr::GET(link_download, httr::write_disk(file.path(
    path_data,
    paste0(version_data, ".zip")),
                                      overwrite = overwrite))
  }

  #Unzip folder
  utils::unzip(zipfile = paste0(file.path(path_data, version_data), ".zip"),
        exdir = file.path(path_data, version_data))

  #Print message
  if(verbose){
  message("Merging data. Please wait a moment...\n") }

  #Merge data
  merge_data(path_data = path_data, version_data = version_data)

  #Print final message
  if(verbose){
  message("Data downloaded and merged successfully. Final data saved in",
              file.path(path_data, version_data, "CompleteBrazilianFlora.rds"))
  }

}
