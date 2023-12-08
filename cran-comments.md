## Resubmission florabr 1.0.1
> Please omit the redundant "Tools for" at the start of your title.

* Removed the redundant "Tools for" from the start of the title.

> Please provide a link to the used dataset (Brazilian Flora 2020 dataset)
to the description field of your DESCRIPTION file in the form
<http:...> or <https:...>
with angle brackets for auto-linking and no space after 'http:' and
'https:', if available.

* Provided a link to the Brazilian Flora 2020 dataset in the Description field of the DESCRIPTION file. The link follows the format <https:...> as recommended.

> \dontrun{} should only be used if the example really cannot be executed
(e.g. because of missing additional software, missing API keys, ...) by
the user. That's why wrapping examples in \dontrun{} adds the comment
("# Not run:") as a warning for the user. Does not seem necessary.
Please replace \dontrun with \donttest.
Please put functions which download data in \donttest{}.

* Functions that download data (`get_florabr` and `load_florabr`) have been appropriately wrapped in `\donttest{}`.

> You write information messages to the console that cannot be easily
suppressed.
It is more R like to generate objects that can be used to extract the
information a user is interested in, and then print() that object.
Instead of cat() rather use message()/warning() or if(verbose)cat(..)
(or maybe stop()) if you really have to write text to the console.
(except for print, summary, interactive functions) -> 
R/check_version.R, R/filter_florabR.R, R/get_florabr.R,
R/get_spat_occ.R, R/helpers.R, R/load_florabR.R

* Updated to utilize `message()` instead of `cat()`, activated only when `verbose = TRUE`, in the following functions: `filter_florabR()`, `get_florabr()`, `get_spat_occ()`, and `merge_data()` (located in R/helper.R).
* The `check_version()` function remains an exception, using direct console messages to inform the user about the latest Brazilian Flora data version.

> Please ensure that your functions do not write by default or in your
examples/vignettes/tests in the user's home filespace (including the
package directory and getwd()). This is not allowed by CRAN policies.
Please omit any default path in writing functions. In your
examples/vignettes/tests you can write to tempdir(). ->
man/get_florabr.Rd, man/load_florabr.Rd

* In the examples and vignettes of `get_florabr` and `load_florabr`, the functions write data to the temporary directory (`tempdir()`).

## Resubmission florabr 1.0.0
> Please omit "+ file LICENSE" and the file itself which is part of R
anyway. It is only used to specify additional restrictions to the GPL
such as attribution requirements.

* Fixed.

## Test environments
* Windows 11, R 4.3.1 (local)
* MacOS 12.6.9, R release (GitHub Actions)
* Windows 10.0.20348, R release (GitHub Actions)
* Ubuntu 22.04.3 LTS, R release (GitHub Actions)
* Ubuntu 22.04.3 LTS, R devel (GitHub Actions)
* Ubuntu 22.04.3 LTS, R oldrel-1 (GitHub Actions)


## R CMD check results
There were no ERRORs:

There were no WARNINGs:

There were 1 NOTE:
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Weverton Trindade <wevertonf1993@gmail.com>'
A Note that reminds CRAN maintainers to check that the submission comes actually from his maintainer and not anybody else.

## Downstream dependencies
There are currently no downstream dependencies for this package.
