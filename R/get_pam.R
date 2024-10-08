#' Get a presence-absence matrix
#' @description
#'  Get a presence-absence matrix of species based on its distribution
#' (states, biomes and vegetation types) according to Flora e Funga do Brasil.
#'
#' @param data (data.frame) a data.frame imported with the
#' \code{\link{load_florabr}} function or generated by either
#' \code{\link{select_species}} or \code{\link{subset_species}} functions
#' @param by_biome (logical) get occurrences by biome. Default = TRUE
#' @param by_state (logical) get occurrences by State. Default = TRUE
#' @param by_vegetation (logical) get occurrences by vegetation type.
#' Default = FALSE
#' @param remove_empty_sites (logical) remove empty sites (sites without any
#' species) from final presence-absence matrix. Default = TRUE
#' @param return_richness_summary (logical) return a data.frame with the number
#' of species in each site. Default = TRUE
#' @param return_spatial_richness (logical) return a SpatVector with the number
#' of species in each site. Default = TRUE
#' @param return_plot (logical) plot map with the number of species in each
#' site.
#' Only works if return_spatial_richness = TRUE. Default = TRUE
#'
#' @return If return_richness_summary and/or return_spatial_richness is set to
#' TRUE, return a list with:
#' - PAM: the presence-absence matrix (PAM)
#' - Richness_summary: a data.frame with the number of species in each site
#' - Spatial_richness: a SpatVector with the number of species in each site
#' (only by State and biome)
#'
#' If return_richness_summary and return_spatial_richness is set to FALSE,
#' return a presence-absence matrix
#' @usage get_pam(data, by_biome = TRUE, by_state = TRUE,
#'                by_vegetation = FALSE, remove_empty_sites = TRUE,
#'                return_richness_summary = TRUE,
#'                return_spatial_richness = TRUE,
#'                return_plot = TRUE)
#' @importFrom terra merge plot intersect unwrap
#' @importFrom stats quantile
#' @importFrom grDevices terrain.colors
#' @export
#' @references
#' Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#'
#' @examples
#' data("bf_data") #Load Flora e Funga do Brasil data
#' #Select endemic and native species of trees with occurrence only in Amazon
#' am_trees <- select_species(data = bf_data,
#'                           include_subspecies = FALSE,
#'                           include_variety = FALSE,
#'                           kingdom = "Plantae",
#'                           group = "All", subgroup = "All",
#'                           family = "All", genus = "All",
#'                           lifeForm = "Tree", filter_lifeForm = "only",
#'                           habitat = "All", filter_habitat = "in",
#'                           biome = "Amazon",
#'                           filter_biome = "only",
#'                           state = "All", filter_state = "and",
#'                           vegetation = "All",
#'                           filter_vegetation = "in",
#'                           endemism = "Endemic", origin = "Native",
#'                           taxonomicStatus = "Accepted",
#'                           nomenclaturalStatus = "All")
#' #Get presence-absence matrix
#' pam_am <- get_pam(data = am_trees, by_biome = TRUE, by_state = TRUE,
#'                  by_vegetation = FALSE, remove_empty_sites = TRUE,
#'                  return_richness_summary = TRUE,
#'                  return_spatial_richness = TRUE,
#'                  return_plot = TRUE)
#'
get_pam <- function(data, by_biome = TRUE, by_state = TRUE,
                    by_vegetation = FALSE, remove_empty_sites = TRUE,
                    return_richness_summary = TRUE,
                    return_spatial_richness = TRUE,
                    return_plot = TRUE) {
  if (missing(data)) {
    stop("Argument data is not defined")
  }

  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ", class(data)))
  }

  if (!is.logical(by_biome)) {
    stop(paste0("Argument by_biome must be logical, not ", class(by_biome)))
  }

  if (!is.logical(by_state)) {
    stop(paste0("Argument by_state must be logical, not ", class(by_state)))
  }

  if (!is.logical(by_vegetation)) {
    stop(paste0("Argument by_vegetation must be logical, not ",
                class(by_vegetation)))
  }

  if (!is.logical(remove_empty_sites)) {
    stop(paste0("Argument remove_empty_sites must be logical, not ",
                class(remove_empty_sites)))
  }

  if (!is.logical(return_spatial_richness)) {
    stop(paste0("Argument return_spatial_richness must be logical, not ",
                class(return_spatial_richness)))
  }

  if (!is.logical(return_richness_summary)) {
    stop(paste0("Argument return_richness_summary must be logical, not ",
                class(return_richness_summary)))
  }

  #Check colnames in data
  if(!all(c("species", "states", "biome") %in%
          colnames(data))) {
    stop("Important columns are missing in data. Check if data is an object
         created by 'load_florabr()', 'subset_species()' or 'select_species()'")
  }

  #Check if there is at least one TRUE in states or biomes or vegetation
  if(!by_state & !by_biome & !by_vegetation){
    stop("At least one of the parameters by_state, by_biome or by_vegetation must be TRUE")
  }

  # Return_spatial_richnessnly works if by_state or by_biome is set to TRUE
  if(return_spatial_richness & !by_state & !by_biome){
    stop("return_spatial_richness=TRUE only works if by_state or/and by_biome is set to TRUE")
  }

  #Get columns
  columns <- c("species")
  if (by_biome) {
    columns <- c(columns, "biome")
  }
  if (by_state) {
    columns <- c(columns, "states")
  }
  if (by_vegetation) {
    columns <- c(columns, "vegetation")
  }
  d <- data[, columns, drop = FALSE]

  #Create list of unique values
  v <- colnames(d)[colnames(d)!= "species"]
  l <- lapply(v, function(i){
    unique(unlist(strsplit(d[,i], ";")))
  })
  names(l) <- v

  # All combinations of state/biomes/vegetations
  sites <- expand.grid(l)
  #Remove NA from states
  if(by_state){
  sites <- subset(sites, sites$states != "NA") }

  # Create an empty presence-absence matrix
  presence_matrix <- matrix(0, nrow = nrow(sites), ncol = nrow(d))


  # Fill matrix with values of presence (1) and absence (0)
  for (i in 1:nrow(d)) {
    species_i <- d[i, "species"]

    #Get index
    site_index <- lapply(v, function(z){
      which(sites[,z] %in% unlist(strsplit(
        d[,z][which(d$species == species_i)], ";")))
    })
    site_index <- Reduce(intersect, site_index)
    presence_matrix[site_index, i] <- 1
  }

  # Name species in the column names
  colnames(presence_matrix) <- d$species

  #Append sites
  pam <- cbind(sites, presence_matrix)

  #Remove empty sites
  if(remove_empty_sites){
  remove_sites <- which(rowSums(pam[,-match(colnames(sites),
                                            colnames(pam))]) == 0)
  if(length(remove_sites) > 0){
  pam <- pam[-remove_sites,]
  }
  }


  ####Richness summary####
  if(return_richness_summary){
    r_sum <- cbind(pam[, intersect(names(pam), v), drop = FALSE],
        richness = rowSums(pam[, setdiff(names(pam), v), drop = FALSE]))}

  ####Spatalize richness####
  if(return_spatial_richness) {
    #Load data
    if(by_state & !by_biome) {
      m <- terra::unwrap(florabr::states)
      names(m)[1] <- "states"
    }
    if(by_biome & !by_state) {
      m <- terra::unwrap(florabr::biomes)
      names(m) <- "biome"
    }

    #If by_biome and by_state = TRUE, intersect polygons
    if(by_biome & by_state){
    m <- terra::intersect(terra::unwrap(florabr::states),
                          terra::unwrap(florabr::biomes))
    names(m)[1] <- "states"
    names(m)[4] <- "biome"
    }

    #Get columns
    v2 <- v[which(v != "vegetation")]
    id_pam <- unique(pam[, v2, drop = FALSE])

    #Update pam to deal only with states and biomes
    if(by_vegetation) {
      pam_m <- pam[,-match("vegetation", colnames(pam))]
      if(by_biome & !by_state) {
      pam_m <- aggregate(. ~ biome, data = pam_m, FUN = sum) }
      if(by_state & !by_biome){
      pam_m <- aggregate(. ~ states, data = pam_m, FUN = sum)
      }

      if(by_biome & by_state) {
        pam_m <- aggregate(. ~ biome + states, data = pam_m, FUN = sum)
      }
      pam_m[d$species] <- lapply(pam_m[d$species], function(x)
        replace(x, x > 1, 1)) } else {
        pam_m <- pam
      }

    #Calculate richness
    r <- rowSums(pam_m[,d$species])

    #Create and transfer ID
    pam_m$site_id <- 1:nrow(pam_m)

    #Get new dataframe with richness by site
    pam_m <- cbind(pam_m[, v2, drop = FALSE], richness = r)


    #####Create complete combination of states and biomes####
    pam_m <- merge(m[[v2]], pam_m, by = v2, all.x = TRUE, all.y = FALSE)

    #Merge with spatial data
    m <- terra::merge(m, pam_m, by = v2, na.rm = FALSE)

    if(return_plot){
      n_breaks <- ifelse(nrow(pam_m) <= 10, nrow(pam_m), 10)
      set_breaks <- stats::quantile(1:max(pam_m$richness, na.rm = TRUE),
                             probs = seq(0, 1, length.out = n_breaks + 1))
      plot_title <- ifelse(by_state & !by_biome, "Richness by State", ifelse(
        !by_state & by_biome, "Richness by biome", ifelse(
          by_state & by_biome, "Richness by State and biome", NA)
      ))

      if(n_breaks >= 10){
      terra::plot(m, "richness", breaks = set_breaks,
                col = rev(grDevices::terrain.colors(length(set_breaks))),
                main = plot_title) }
      else{
        terra::plot(m, "richness", col = rev(grDevices::terrain.colors(n_breaks)),
                    main = plot_title)
      }
    }
    } #End of return_spatial_richness

  #Return final data
  if(!return_richness_summary) {
    r_sum <- NA
  }
  if(!return_spatial_richness){
    m <- NA
  }


  #Crate final list
  res <- list(PAM = pam,
              Richness_summary = r_sum,
              Spatial_richness = m)

  res <- res[!is.na(res)]

  if(length(res) > 1){
    return(res)
  } else {
    return(pam)
  }
  }#End of function

