# rdbnomics 0.4.5

* The vignette "rdbnomics-tutorial" is now named "rdbnomics".
* `rdb()` and `rdb_...()` functions now have an argument to configure curl. It
  is also an option of the package. The vignette "rdbnomics" is
  modified consequently.
* `rdb()` and `rdb_by_api_link()` functions return an error if the columns
  `period` and `period_start_day` don't exist.
* `rdb()` function passes argument `provider_code` to `ids` if only
  `provider_code` is provided. It is considered that the user wants to use
  `ids` without naming the arguments.
* `rdb()` function passes argument `ids` to `mask` if only
  `provider_code`, `dataset_code` and `ids` are provided. It is considered that
   the user wants to use `mask` without naming the argument.

# rdbnomics 0.4.4

* Change of the API version from 21 to 22.
* The vignette "rdbnomics-tutorial" is modified to be backward compatible
  with R Markdown v1 (i.e. without pandoc or rmarkdown).
* The vignette "rdbnomics-tutorial" has an appendix.
* New `rdb_providers()` returns the available providers.
* New `rdb_last_updates()` shows the last updates.
* In the functions, dates and timestamps are transformed using `as.Date()` and
  `as.POSIXct()`.
* `dimensions` in `rdb()` can be a list (@Iwo, #1).

# rdbnomics 0.4.3

* First release on CRAN.
