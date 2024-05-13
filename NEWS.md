# florabr 1.2.0 (May 2024)

## New Features and Enhancements

- Dataset names have been updated in functions and documentation to reflect the new phase of the project, now known as Flora e Funga do Brasil.
- Introducing the `get_synonym()` function, which retrieves synonyms for species.
- Introducing the `solve_discrepancies()` function, designed to resolve inconsistencies between species and subspecies/varieties information.
- The `get_florabr()` function now integrates the solve_discrepancies() function to address discrepancies when the solve_discrepancy parameter is set to TRUE.
- Fixed a bug with `get_binomial` related to species whose Genus or Specific epithet contains '-' or '×' characters.
- Improved handling of whitespace in `get_binomial`, addressing leading, trailing, and between words.
- The filtering procedure in `select_species()` and `get_attributes()` is now case insensitive, treating uppercase and lowercase letters as equivalent.
- Updated data example (`bf_data`) to use a subset of version 393.401 of the Brazilian Flora database.
- `get_attributes()` now supports more than one attribute and returns a list with the available options for each attribute.
- Removed `Attributes` data, as it is no longer necessary for `get_attributes()` to function.
- Short version of data loaded with `load_florabr` now incorporates three additional columns: phylum, class, and order.
- The `select_species` function now allows filtering by phylum, class, and order.
- Updated README, DESCRIPTION and vignettes to include descriptions of the new functions and modifications.

## Testing

- Added `test_that` statements for all functions.


# florabr 1.1.0 (March 2024)

* get_florabr now fixes inconsistencies between species and subspecies/varieties information
* Introducing `get_pam` function, which converts a dataframe containing species distribution information into a presence-absence matrix.
* Added a new vignette providing a detailed description of the `get_pam` function and its usage.

# florabr 1.0.0 (November 2023)

* Initial CRAN submission.
