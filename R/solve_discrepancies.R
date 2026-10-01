#' Resolve discrepancies between species and subspecies/varieties information
#'
#' @param data (data.frame) the data.frame imported with the
#' \code{\link{load_florabr}} function.
#'
#' @return a data.frame with the discrepancies solved
#' @usage solve_discrepancies(data)
#' @details
#' In the original dataset, discrepancies may exist between species and
#' subspecies/varieties information. An example of a discrepancy is when a
#' species occurs only in one biome (e.g., Amazon), but a subspecies or variety
#' of the same species occurs in another biome (e.g., Cerrado). This function
#' rectifies such discrepancies by considering distribution (states, biomes,
#' and vegetation), life form, and habitat. For instance, if a subspecies is
#' recorded in a specific biome, it implies that the species also occurs in that
#' biome.
#'
#' @importFrom data.table copy as.data.table set
#'
#' @export
#'
#' @examples
#' data("bf_data") #Load Flora e Funga do Brasil data
#' #Check if discrepancies were solved in the dataset
#' attr(bf_data, "solve_discrepancies")
#' #Solve discrepancies
#' bf_solved <- solve_discrepancies(bf_data)
#' #Check if discrepancies were solved in the dataset
#' attr(bf_solved, "solve_discrepancies")
solve_discrepancies <- function(data) {
  if (missing(data) || !inherits(data, "data.frame")) {
    stop("data must be a data.frame or data.table.", call. = FALSE)
  }

  if (isTRUE(attr(data, "solve_discrepancies"))) {
    stop(
      "Discrepancies have already been resolved in this dataset.",
      call. = FALSE
    )
  }

  required_columns <- c(
    "id", "species", "taxonRank", "taxonomicStatus", "endemism",
    "lifeForm", "habitat", "biome", "states", "vegetation"
  )

  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  # Work on a copy: data.table updates columns by reference.
  result <- data.table::copy(data.table::as.data.table(data))

  if (nrow(result) == 0L) {
    output <- as.data.frame(result)
    attr(output, "solve_discrepancies") <- TRUE
    return(output)
  }

  binomial <- get_binomial(
    species_names = as.character(result[["species"]]),
    include_variety = FALSE,
    include_subspecies = FALSE
  )

  valid_binomial <- !is.na(binomial) & nzchar(binomial)
  rank <- result[["taxonRank"]]
  status <- result[["taxonomicStatus"]]
  endemism <- result[["endemism"]]

  species_rows <- which(
    valid_binomial &
      rank == "Species" &
      status == "Accepted"
  )

  # Do not propagate information from taxa marked as absent from Brazil.
  infra_rows <- which(
    valid_binomial &
      rank %in% c("Subspecies", "Variety", "Form") &
      status == "Accepted" &
      !is.na(endemism) &
      endemism != "Not_found_in_brazil"
  )

  infra_names <- unique(binomial[infra_rows])
  species_names <- unique(binomial[species_rows])
  matching_names <- intersect(infra_names, species_names)
  missing_parents <- setdiff(infra_names, species_names)

  if (length(missing_parents) > 0L) {
    message(
      "Skipped ", length(missing_parents),
      " infraspecific group(s) without an accepted species record."
    )
  }

  columns_to_update <- c(
    "lifeForm", "habitat", "biome", "states", "vegetation"
  )

  if (length(matching_names) > 0L) {
    # Include existing species information and accepted infraspecific
    # information in each aggregation.
    contributing_rows <- c(
      species_rows[binomial[species_rows] %in% matching_names],
      infra_rows[binomial[infra_rows] %in% matching_names]
    )

    source <- result[
      contributing_rows, columns_to_update, with = FALSE
    ]
    data.table::set(
      source,
      j = "species_bin",
      value = binomial[contributing_rows]
    )

    collapse_values <- function(values) {
      tokens <- unlist(
        strsplit(as.character(values), ";", fixed = TRUE),
        use.names = FALSE
      )
      tokens <- trimws(tokens)
      tokens <- unique(tokens[!is.na(tokens) & nzchar(tokens)])

      # A known value takes precedence over "Unknown" or
      # "Not_found_in_brazil".
      known <- setdiff(
        tokens,
        c("Unknown", "Not_found_in_brazil")
      )
      if (length(known) > 0L) {
        return(paste(sort(known), collapse = ";"))
      }

      if ("Unknown" %in% tokens) {
        return("Unknown")
      }
      if ("Not_found_in_brazil" %in% tokens) {
        return("Not_found_in_brazil")
      }

      "Unknown"
    }

    aggregated <- source[
      ,
      list(
        lifeForm = collapse_values(get("lifeForm")),
        habitat = collapse_values(get("habitat")),
        biome = collapse_values(get("biome")),
        states = collapse_values(get("states")),
        vegetation = collapse_values(get("vegetation"))
      ),
      by = "species_bin"
    ]

    target_rows <- species_rows[
      binomial[species_rows] %in% matching_names
    ]
    aggregate_index <- match(
      binomial[target_rows],
      aggregated[["species_bin"]]
    )

    for (column in columns_to_update) {
      data.table::set(
        result,
        i = target_rows,
        j = column,
        value = aggregated[[column]][aggregate_index]
      )
    }
  }

  # Remove "Unknown" only when another value exists in the same field.
  # Unlike the previous gsub(), this does not damage individual tokens.
  for (column in c(
    columns_to_update, "origin", "endemism"
  )) {
    values <- result[[column]]
    affected <- which(
      !is.na(values) &
        grepl("Unknown", values, fixed = TRUE) &
        grepl(";", values, fixed = TRUE)
    )

    if (length(affected) > 0L) {
      values[affected] <- vapply(
        strsplit(values[affected], ";", fixed = TRUE),
        function(parts) {
          parts <- trimws(parts)
          parts <- parts[nzchar(parts)]

          if ("Unknown" %in% parts && length(parts) > 1L) {
            parts <- parts[parts != "Unknown"]
          }

          paste(parts, collapse = ";")
        },
        character(1),
        USE.NAMES = FALSE
      )
      data.table::set(result, j = column, value = values)
    }
  }

  # Preserve the documented return class and input row order.
  output <- as.data.frame(result)
  attr(output, "solve_discrepancies") <- TRUE
  output
}
