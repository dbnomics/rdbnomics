# rdbnomics 0.4.5

* `rdb()` and `rdb_...()` functions now have an argument to configure curl. It
  is also an option of the package. The vignette "rdbnomics-tutorial.Rmd" is
  modified consequently.

# rdbnomics 0.4.4

* Change of the API version from 21 to 22.
* The vignette "rdbnomics-tutorial.Rmd" is modified to be backward compatible
  with R Markdown v1 (i.e. without pandoc or rmarkdown).
* The vignette "rdbnomics-tutorial.Rmd" has an appendix.
* New `rdb_providers()` returns the available providers.
* New `rdb_last_updates()` shows the last updates.
* In the functions, dates and timestamps are transformed using `as.Date()` and
  `as.POSIXct()`.
* `dimensions` in `rdb()` can be a list (@Iwo, #1).

# rdbnomics 0.4.3

* First release on CRAN.
