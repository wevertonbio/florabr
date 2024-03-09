#' Load Brazilian Flora database
#'
#' @param data_dir (character) the same directory used to save the data
#' downloaded from Brazilian Flora 2020 using the \link{get_florabr} function.
#' @param data_version (character) the version of Brazilian Flora database to
#' be loaded. It can be "Latest_available", which will load the latest version
#' available; or another specified version, for example "393.364".
#' Default = "Latest_available".
#' @param type (character) it determines the number of columns that will be
#' loaded. It can be "short" or "complete". Default = "short". See details.
#' @param verbose (logical) Whether to display messages during function
#' execution. Set to TRUE to enable display, or FALSE to run silently.
#' Default = TRUE.
#' @details
#' The parameter type accepts two arguments. If type = short, it will load a
#' data.frame with the 19 columns needed to run the other functions of the
#' package: species, scientificName, acceptedName, kingdom, Group, Subgroup,
#' family, genus, lifeForm, habitat, Biome, States, vegetationType, Origin,
#' Endemism, taxonomicStatus, nomenclaturalStatus, vernacularName, and
#' taxonRank.
#' If type = complete, it will load a data.frame with all 39 variables available
#'  in Brazilian Flora database.
#'
#' @return A data.frame with the specified version (Default is the latest
#' available) of the Brazilian Flora database. This data.frame is necessary to
#' run most of the functions of the package.
#'
#' @usage load_florabr(data_dir, data_version = "Latest_available",
#'                     type = "short", verbose = TRUE)
#' @export
#' @references
#' Brazilian Flora 2020. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#'
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
#' #Load data
#' df <- load_florabr(data_dir = my_dir, data_version = "Latest_available",
#' type = "short")
#' }
load_florabr <- function(data_dir, data_version = "Latest_available",
                         type = "short", verbose = TRUE){
  #Set folder
  if (missing(data_dir)) {
    stop("Argument data_dir is not defined")
  }

  #Check classes
  if (!is.character(data_dir)) {
    stop(paste0("Argument data_dir must be a character, not ", class(data_dir)))
  }

  if (!is.character(data_version)) {
    stop(paste0("Argument data_version must be a character, not ",
                class(data_version)))
  }

  if (!(type %in% c("short", "complete"))) {
    stop("Argument type must be 'short' or 'complete'")
  }


#Set directory
  path_data <-  data_dir

  #Get latest available version if version was not set
  if(data_version == "Latest_available") {
    all_dirs <- list.dirs(path = path_data, recursive = FALSE,
                          full.names = FALSE)
    dir_versions <- subset(all_dirs, grepl("393", all_dirs)) #Actual version
    #Get highest version
    if(length(dir_versions) > 0) {
      high_version <- max(as.numeric(gsub("393.", "", dir_versions)))
      version_data <- paste0("393.", high_version) } else {
        version_data <- 0
      } } else {version_data <-  data_version}
  #Stop if version_data = 0
  if (version_data == 0) {
    stop("There is no version of Flora do Brasil in the specified directory.
         Please check the directory or run the 'get_latest_version()' function
         to download the latest version of the data")
  }

  #Load data
  if(verbose){
  message("Loading version", version_data) }

  if(type == "complete") {
    ds <- readRDS(file.path(path_data, version_data,
                            "CompleteBrazilianFlora.rds")) }

  if(type == "short") {
    ds <- readRDS(file.path(path_data, version_data,
                            "CompleteBrazilianFlora.rds"))
    ds <- ds[,c("species", "scientificName", "acceptedName", "kingdom", "Group",
                "Subgroup", "family",
                "genus", "lifeForm",
             "habitat", "Biome", "States", "vegetationType", "Origin",
             "Endemism",
             "taxonomicStatus", "nomenclaturalStatus", "vernacularName",
             "taxonRank")]
  }
  return(ds)
}

