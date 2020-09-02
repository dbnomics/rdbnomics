# rdbnomics 0.6.3

* New badge in README.
* Correction of .gitlab-ci.yml with pkg-config.
* Correction of `rdb()` examples in the doc and README.

# rdbnomics 0.6.2

* Remove **ggplot2**, **DT** and **dplyr** vignette dependencies.
* Internal function `get_data` better handles errors.

# rdbnomics 0.6.1

* New arguments `dimensions` and `query` for the function `rdb_series()`.

# rdbnomics 0.6.0

* New function `rdb_datasets()` to request the available datasets of the
  providers (@fmgithub2017, #3 github).
* New function `rdb_dimensions()` to request the list of the dimensions of
  the available datasets of the providers (@fmgithub2017, #3 github).
* New function `rdb_series()` to request the list of the series of
  the available datasets of the providers (@fmgithub2017, #3 github).
* Add a clearer error message to the function to_xts() in README (Martin
  Feldkircher, 20200326 email).
* Update vignette for `rdb_datasets()`, `rdb_dimensions()` and `rdb_series()`.

# rdbnomics 0.5.2

* Correction of a bug in the internal function `deploy`. The cases
  `to_list_length = 1` and `to_list_length = 2` are now well handled
  (@julia.schmidt, 20200205 forum.db.nomics.world).

# rdbnomics 0.5.1

* New argument 'query' for function `rdb()`.
* New argument 'api_link' for function `rdb()` to replace `rdb_by_api_link()`.
* New internal function `.rdb()` because `rdb_by_api_link()` is deprecated.
* New function `dbnomics()` which is used in the vignette (@blu2ego, #2 github).
* Small class correction for R 3.1.

# rdbnomics 0.5.0

* New filters tool from <https://editor.nomics.world/filters>.
* If the retrieved dataset contains columns with codes (like ISO codes,
  geographic codes, ...), then correspondences are performed to translate
  these codes if possible.
* Internal function `get_data` better handles errors. The message is clearer.

# rdbnomics 0.4.7

* Use of `metadata` from the API to download less data.

# rdbnomics 0.4.6

* Internal function `deploy` better handles recursive lists.

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
* `dimensions` in `rdb()` can be a list (@Iwo, #1 gitlab).

# rdbnomics 0.4.3

* First release on CRAN.
