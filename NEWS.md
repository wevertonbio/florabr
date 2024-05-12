# florabr 1.2.0 (May 2024)

## New Features and Enhancements

- Introducing the `get_synonym()` function, which retrieves synonyms for species.
- Introducing the `solve_discrepancies()` function, designed to resolve inconsistencies between species and subspecies/varieties information.
- The `get_florabr()` function now integrates the solve_discrepancies() function to address discrepancies when the solve_discrepancy parameter is set to TRUE.
- Fixed a bug with `get_binomial` related to species whose Genus or Specific epithet contains '-' or '×' characters.
- Improved handling of whitespace in `get_binomial`, addressing leading, trailing, and between words.
- The filtering procedure in `select_species()` and `get_attributes()` is now case insensitive, treating uppercase and lowercase letters as equivalent.
- Updated data example (`bf_data`) to use a subset of version 393.401 of the Brazilian Flora database.
- Removed `Attributes` data, as it is no longer necessary for `get_attributes()` to function.
- Updated README, DESCRIPTION and vignettes to include descriptions of the new functions and modifications.

## Testing

- Added `test_that` statements for all functions.


# florabr 1.1.0 (March 2024)

* get_florabr now fixes inconsistencies between species and subspecies/varieties information
* Introducing `get_pam` function, which converts a dataframe containing species distribution information into a presence-absence matrix.
* Added a new vignette providing a detailed description of the `get_pam` function and its usage.

# florabr 1.0.0 (November 2023)

* Initial CRAN submission.
