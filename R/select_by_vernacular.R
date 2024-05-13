#' Search for taxa using vernacular names
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function or generated with the function
#' \code{\link{select_species}}.
#' @param names (character) vernacular name ("Nome comum") of the species to be
#' searched
#' @param exact (logic) if TRUE, the function will search only for exact
#' matches. For example, if names = "pinheiro" and exact = TRUE, the function
#' will return only the species popularly known as "pinheiro". On the other
#' hand, if names = "pinheiro" and exact = FALSE, the function will return
#' other results as "pinheiro-do-parana". Default = FALSE
#'
#' @return a data.frame with the species with vernacular names that match the
#' input names
#' @export
#' @references
#' Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#' @examples
#' data("bf_data") #Load Flora e Funga do Brasil data
#' #Search for species whose vernacular name is 'pinheiro'
#' pinheiro_exact <- select_by_vernacular(data = bf_data,
#'                                        names = "pinheiro",
#'                                        exact = TRUE)
#' pinheiro_exact
#' #Search for species whose vernacular name is 'pinheiro', allowing non-exact
#' #matches
#' pinheiro_not_exact <- select_by_vernacular(data = bf_data,
#'                                           names = "pinheiro",
#'                                           exact = FALSE)
#' head(pinheiro_not_exact)
#'
#' @references
#' Flora e Funga do Brasil. Jardim Botânico do Rio de Janeiro. Available at:
#' http://floradobrasil.jbrj.gov.br/
#'
select_by_vernacular <- function(data, names,
                                 exact = FALSE){
  if (missing(data)) {
    stop("Argument data is not defined")
  }

  if (missing(names)) {
    stop("Argument names is not defined")
  }
  #Check classes
  if (!inherits(data, "data.frame")) {
    stop(paste0("Argument data must be a data.frame, not ", class(data)))
  }

  if (!is.character(names)) {
    stop(paste0("Argument names must be a character, not ", class(names)))
  }

  if (!is.logical(exact)) {
    stop(paste0("Argument exact must be logical, not ", class(exact)))
  }


  #Lower case
  names <- tolower(names)

  #Subset
  dv <- subset(data, grepl(paste0(
    paste0("\\b", gsub("\\s", " ", names), "\\b"),
    collapse = "|"), data$vernacularName))


  if(exact) {
  has_word <- function(n) {
      palavras <- unlist(strsplit(n, ", "))
      any(grepl(paste0("^", names, "$"), palavras))
    }
  dv <- dv[apply(dv, 1, function(x) has_word(x["vernacularName"])), ]
  }

  if(nrow(dv) == 0) {
    stop(paste("There isn't any species in the Flora e Funga do Brasil with this",
    "\nvernacular name"))
  }

  return(dv)
}

#select_by_vernacular(data = d, names = "pimenta", exact = T)



