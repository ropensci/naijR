# naijR 0.6.0
* Added a new dataset for the States of Nigeria.
* Improved on the accuracy of LGA naming in the light of spelling mistakes discovered in the earlier reference document.
* Migration from RGDAL-based spatial data; the `{sf}` package is the new main dependency.

## Fixes:
* Unwanted display of dialogs when fixing LGA names on Windows OS was reversed (suggested by Laura DeCicco).

# naijR 0.5.2
* Quietened a verbose warning introduced via the soon-to-be-retired spatial data packages.

# naijR 0.5.1
* In nested calls with the function `fix_region`, the `lgas` constructor function does not warn if there are spelling mistakes, as this turned out to be a bit confusing when it was used. In earlier versions, warnings persisted even after fixes were applied.
* In carrying out interactive fixes, particularly of LGA spellings, the more familiar and intuitive native Windows messaging and dialog system is used. Works only on Windows machines; on Linux and MacOs, the usual messaging and interaction occurs at the R console.
* Control the size of map labels with the `cex` argument (passed on to `maps::map.text` internally, via `...`).
* Handle instances where the term *Abuja* is used as a State (which technically it is not) and signal a warning to the user.

# naijR 0.5.0
## New features:
* New methods for `?InternalGenerics` were introduced e.g. for `c()`, `[`, `[[`, `na.exclude`, etc.

## Enhancements:
* Effectively handle mobile numbers that have common separators in them, namely whitespace, '-' or '.'.
* Repair mobile numbers where poor data entry interchanges zeros (`0`s) with the letter `O` (works for both upper and lower case).
* Repair of mobile numbers now offers optional information for users.
* Allow the use of factor input when creating objects of class `regions`.
* Added a new argument `legend.text` for `map_ng` using an idiom that is similar to the one used in `base::barplot`, thanks to observations made by @VictoriaLatham in issue #27.
* Simplified the creation of choropleth maps with 2-column data frames; one of the columns is to be a vector of valid States or Local Government Areas, and the other a factor or something coercible to one. 

## Bug fixes:
* `map_ng` accepted arguments that were not `data.frame`s leading to unwieldy errors. It is now made sure to fail early in such cases

## Deprecated:
* Arguments of `map_ng` - `leg.x`, `leg.y`, and `leg.orient` were marked for deprecation in the next minor release.

# naijR 0.4.4
## Bug fix:
* `fix_mobile` fails unexpectedly when only `NA` is supplied as argument. This causes practical problems when, for example, it encounters a column with only missing values.

# naijR 0.4.3
* Addressed a build problem related to CRAN submission.

# naijR 0.4.2
* Improved type checking for mapping functionality and better fidelity.

# naijR 0.4.1
* Enable the exclusion of selected States from a choropleth map (#27).
* Cleaner output for `states` and `lgas` objects.

# naijR 0.4.0
* Introduce the ability to 'manually' fix names of States or LGAs.
* Update the documentation with a new vignette.

# naijR 0.3.4
* Fixed package-wide misuse of the word _Nasarawa_.

# naijR 0.3.3
* Fixed repetitions in the output when multiple LGAs' spellings are corrected.

# naijR 0.3.2
* Improved on print methods

# naijR 0.3.1
* Fixed a bug that affected the proper rendering of LGA-level maps for some of the States. The approach used was to simply filter the entire data when requiring a State map, so as to reduce name clashes that occurred from synonyms amongst some of the LGAs and/or States.
* Enabled the fine-tuning of creation of `lgas` objects in the event that the argument provided is the name of an LGA that is synonymous with it's State (argument `strict`).

# naijR 0.3.0
* Provide new methods for the S3 generics `head` and `tail` to work with objects that inherit from class `regions`.
* The S3 constructors `states` and `lgas` gain a logical argument `warn` to control whether or not they issue a warning when an input string does not contain an actual State/LGA.
* General improvement of the formatting of output to enhance the user experience.
* Fixed a bug that prevented the loading of LGAs from the internal data when the package is not attached to the search path i.e. invocation with `naijR::lgas()` was producing an error.

# naijR 0.2.2
* Export S3 generic `fix_region`.

# naijR 0.2.1
* Fixed incorrect URLs, as noted by CRAN
* Edits to output message

# naijR 0.2.0
* Added a new function `is_lga`, which checks an object for Local Government Areas.
* Ignore, with a warning, the check for `is_state` when the object checked is not of type `character`.
* Draw maps up to LGA level

# naijR 0.1.5
* Built new package website.

# naijR 0.1.4
* Suppress deprecation warning for `is_state` when it is called internally by package function; displayed only when function is called directly.

# naijR 0.1.3
* Added a `NEWS.md` file to track changes to the package.
* Recognise abbreviations of 'Federal Capital Territory' i.e. FCT.
* Disable error-check on character type for `is_state` so it can be used more effectively for functional programming constructs.
