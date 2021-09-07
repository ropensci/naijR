# v0.3.0
* Provide new methods of `head`, `tail` to work with objects that inherit from class `regions`.

# v0.2.2
* Export S3 generic `fix_region`.

# v0.2.1
* Fixed incorrect URLs, as noted by CRAN
* Edits to output message

# v0.2.0
* Added a new function `is_lga`, which checks an object for Local Government Areas.
* Ignore, with a warning, the check for `is_state` when the object checked is not of type `character`.
* Draw maps up to LGA level

# v0.1.5
* Built new package website.

# v0.1.4
* Suppress deprecation warning for `is_state()` when it is called internally by package function; displayed only when function is called directly.

# v0.1.3
* Added a `NEWS.md` file to track changes to the package.
* Recognise abbreviations of 'Federal Capital Territory' i.e. FCT.
* Disable error-check on character type for `is_state` so it can be used more effectively for functional programming constructs.
