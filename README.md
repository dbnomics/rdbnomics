# rdbnomics <img src="man/figures/logo.png" align="right" width="120" />

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rdbnomics)](https://cran.r-project.org/package=rdbnomics)
[![](http://cranlogs.r-pkg.org/badges/grand-total/rdbnomics)](https://cran.r-project.org/package=rdbnomics)
[![pipeline status](https://git.nomics.world/dbnomics/rdbnomics/badges/master/pipeline.svg)](https://git.nomics.world/dbnomics/rdbnomics/commits/master)
[![status](https://tinyverse.netlify.com/badge/rdbnomics)](https://CRAN.R-project.org/package=rdbnomics)

## DBnomics R client

This package provides you access to DBnomics data series. DBnomics is an open-source project with the goal of aggregating the world's economic data in one location, free of charge to the public. DBnomics covers hundreds of millions of series from international and national institutions (Eurostat, World Bank, IMF, ECB, ...).

To use this package, you have to provide the codes of the provider, dataset and series you want. You can retrieve them directly on the <a href="https://db.nomics.world/" target="_blank">website</a>. You have access to the API through this <a href="http://api.db.nomics.world/" target="_blank">link</a> and the documentation is <a href="https://api.db.nomics.world/v22/apidocs" target="_blank">here</a>.

DBnomics is hosted on its own <a href="https://git.nomics.world/" target="_blank">gitlab platform</a>. However, in order to install the package more easily, we created a mirror of this package on <a href="https://github.com/dbnomics/rdbnomics" target="_blank">github</a>.

To install `rdbnomics` from CRAN:

```r
install.packages("rdbnomics")
library(rdbnomics)
```

To install `rdbnomics` from github:

```r
remotes::install_github("dbnomics/rdbnomics", build_vignettes = TRUE, force = TRUE)
library(rdbnomics)
```

After installation, a vignette is available to the user:
```r
vignette("rdbnomics")
```

## Examples
### Fetch time series by `ids`:
```r
# Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df1 <- rdb(ids = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN")

# Fetch two series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df2 <- rdb(ids = c("AMECO/ZUTN/EA19.1.0.0.0.ZUTN", "AMECO/ZUTN/DNK.1.0.0.0.ZUTN"))

# Fetch two series from different datasets of different providers:
df3 <- rdb(ids = c("AMECO/ZUTN/EA19.1.0.0.0.ZUTN", "IMF/BOP/A.FR.BCA_BP6_EUR"))
```

In the event that you only use the argument `ids`, you can drop it and run:
```r
df <- rdb("AMECO/ZUTN/EA19.1.0.0.0.ZUTN")
```

### Fetch time series by `mask` :
```r
# Fetch one series from dataset 'Balance of Payments' (BOP) of IMF:
df1 <- rdb("IMF", "BOP", mask = "A.FR.BCA_BP6_EUR")

# Fetch two series from dataset 'Balance of Payments' (BOP) of IMF:
df2 <- rdb("IMF", "BOP", mask = "A.FR+ES.BCA_BP6_EUR")

# Fetch all series along one dimension from dataset 'Balance of Payments' (BOP) of IMF:
df3 <- rdb("IMF", "BOP", mask = "A..BCA_BP6_EUR")

# Fetch series along multiple dimensions from dataset 'Balance of Payments' (BOP) of IMF:
df4 <- rdb("IMF", "BOP", mask = "A.FR.BCA_BP6_EUR+IA_BP6_EUR")
```

In the event that you only use the arguments `provider_code`, `dataset_code` and `mask`, you can drop the name `mask` and run:
```r
df4 <- rdb("IMF", "BOP", "A.FR.BCA_BP6_EUR")
```

### Fetch time series by `dimensions`:
```r
# Fetch one value of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df1 <- rdb("AMECO", "ZUTN", dimensions = list(geo = "ea12"))
# or
df1 <- rdb("AMECO", "ZUTN", dimensions = '{"geo": ["ea12"]}')

# Fetch two values of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df2 <- rdb("AMECO", "ZUTN", dimensions = list(geo = c("ea12", "dnk")))
# or
df2 <- rdb("AMECO", "ZUTN", dimensions = '{"geo": ["ea12", "dnk"]}')

# Fetch several values of several dimensions from dataset 'Doing business' (DB) of World Bank:
df3 <- rdb("WB", "DB", dimensions = list(country = c("DZ", "PE"), indicator = c("ENF.CONT.COEN.COST.ZS", "IC.REG.COST.PC.FE.ZS")))
# or
df3 <- rdb("WB", "DB", dimensions = '{"country": ["DZ", "PE"], "indicator": ["ENF.CONT.COEN.COST.ZS", "IC.REG.COST.PC.FE.ZS"]}')
```

### Fetch time series with a `query`:
```r
# Fetch one series from dataset 'WEO by countries' (WEO) of IMF provider:
df1 <- rdb("IMF", "WEO", query = "France current account balance percent")

# Fetch series from dataset 'WEO by countries' (WEO) of IMF provider:
df2 <- rdb("IMF", "WEO", query = "current account balance percent")
```

### Fetch one series from the dataset 'Doing Business' of WB provider with the link:
```r
df1 <- rdb(api_link = "https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22country%22%3A%5B%22FR%22%2C%22IT%22%2C%22ES%22%5D%7D&q=IC.REG.PROC.FE.NO&observations=1&format=json&align_periods=1&offset=0&facets=0")
```

In the event that you only use the argument `api_link`, you can drop the name and run:
```r
df1 <- rdb("https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22country%22%3A%5B%22FR%22%2C%22IT%22%2C%22ES%22%5D%7D&q=IC.REG.PROC.FE.NO&observations=1&format=json&align_periods=1&offset=0&facets=0")
```

## Fetch the available datasets of a provider
```r
# Example with the IMF datasets:
df_datasets <- rdb_datasets(provider_code = "IMF")

# Example with the IMF and BDF datasets:
df_datasets <- rdb_datasets(provider_code = c("IMF", "BDF"))
```

In the event that you only request the datasets for one provider, if you define
`simplify = TRUE`, then the result will be a `data.table` not a named list.
```r
df_datasets <- rdb_datasets(provider_code = "IMF", simplify = TRUE)
```

## Fetch the possible dimensions of available datasets of a provider
```r
# Example for the dataset WEO of the IMF:
df_dimensions <- rdb_dimensions(provider_code = "IMF", dataset_code = "WEO")
```

In the event that you only request the dimensions for one dataset for one
provider, if you define `simplify = TRUE`, then the result will be a named list
`data.table` not a nested named list.
```r
df_dimensions <- rdb_dimensions(provider_code = "IMF", dataset_code = "WEO", simplify = TRUE)
```

## Fetch the number of series of available datasets of a provider
```r
# Example for the dataset WEOAGG of the IMF:
df_series <- rdb_series(provider_code = "IMF", dataset_code = "WEOAGG")

# With dimensions
df_series <- rdb_series("IMF", "WEO", dimensions = list(`weo-country` = "AGO"))
df_series <- rdb_series("IMF", "WEO", dimensions = list(`weo-subject` = "NGDP_RPCH"), simplify = TRUE)

# With a query
df_series <- rdb_series("IMF", "WEO", query = "ARE")
df_series <- rdb_series("IMF", c("WEO", "WEOAGG"), query = "NGDP_RPCH")
```

:warning: We ask the user to use this function parsimoniously because there are a huge amount
of series per dataset. Please only fetch for one dataset if you need it or
visit the website [https://db.nomics.world](https://db.nomics.world).  

## Proxy configuration or connection error `Could not resolve host`
When using the function `rdb`, you may come across the following error:
```r
Error in open.connection(con, "rb") :
  Could not resolve host: api.db.nomics.world
```
To get round this situation, you have two possibilities:

1. configure **curl** to use a specific and authorized proxy.

2. use the default R internet connection i.e. the Internet Explorer proxy defined in *internet2.dll*.

### Configure **curl** to use a specific and authorized proxy
In **rdbnomics**, by default the function `curl_fetch_memory` (of the package **curl**) is used to fetch the data. If a specific proxy must be used, it is possible to define it permanently with the package option `rdbnomics.curl_config` or on the fly through the argument `curl_config`. Because the object is a named list, its elements are passed to the connection (the `curl_handle` object created internally with `new_handle()`) with `handle_setopt()` before using `curl_fetch_memory`.

To see the available parameters, run `names(curl_options())` in *R* or visit the website <a href="https://curl.haxx.se/libcurl/c/curl_easy_setopt.html" target="_blank">https://curl.haxx.se/libcurl/c/curl_easy_setopt.html</a>. Once they are chosen, you define the curl object as follows:
```r
h <- list(
  proxy = "<proxy>",
  proxyport = <port>,
  proxyusername = "<username>",
  proxypassword = "<password>"
)
```

#### Set the connection up for a session
The curl connection can be set up for a session by modifying the following package option:
```r
options(rdbnomics.curl_config = h)
```
When fetching the data, the following command is executed:
```r
hndl <- curl::new_handle()
curl::handle_setopt(hndl, .list = getOption("rdbnomics.curl_config"))
curl::curl_fetch_memory(url = <...>, handle = hndl)
```
After configuration, just use the standard functions of **rdbnomics** e.g.:
```r
df1 <- rdb(ids = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN")
```
This option of the package can be disabled with:
```r
options(rdbnomics.curl = NULL)
```

#### Use the connection only for a function call
If a complete configuration is not needed but just an "on the fly" execution, then use the argument `curl_config` of the function `rdb`:
```r
df1 <- rdb(ids = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN", curl_config = h)
```

### Use the default R internet connection
To retrieve the data with the default R internet connection, **rdbnomics** will use the base function `readLines`.

#### Set the connection up for a session
To activate this feature for a session, you need to enable an option of the package:
```r
options(rdbnomics.use_readLines = TRUE)
```
And then use the standard function as follows:
```r
df1 <- rdb(ids = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN")
```
This configuration can be disabled with:
```r
options(rdbnomics.use_readLines = FALSE)
```

#### Use the connection only for a function call
If you just want to do it once, you may use the argument `use_readLines` of the function `rdb`:
```r
df1 <- rdb(ids = "AMECO/ZUTN/EA19.1.0.0.0.ZUTN", use_readLines = TRUE)
```

## Transform time series with filters
The **rdbnomics** package can interact with the *Time Series Editor* of DBnomics to transform time series by applying filters to them.  
Available filters are listed on the filters page [https://editor.nomics.world/filters](https://editor.nomics.world/filters).

Here is an example of how to proceed to interpolate two annual time series with a monthly frequency, using a spline interpolation:

```r
filters <- list(
  code = "interpolate",
  parameters = list(frequency = "monthly", method = "spline")
)

df <- rdb(
  ids = c("AMECO/ZUTN/EA19.1.0.0.0.ZUTN", "AMECO/ZUTN/DNK.1.0.0.0.ZUTN"),
  filters = filters
)
```

If you want to apply more than one filter, the `filters` argument will be a list of valid filters:

```r
filters <- list(
  list(
    code = "interpolate",
    parameters = list(frequency = "monthly", method = "spline")
  ),
  list(
    code = "aggregate",
    parameters = list(frequency = "bi-annual", method = "end_of_period")
  )
)

df <- rdb(
  ids = c("AMECO/ZUTN/EA19.1.0.0.0.ZUTN", "AMECO/ZUTN/DNK.1.0.0.0.ZUTN"),
  filters = filters
)
```

The `data.table` columns change a little bit when filters are used. There are two new columns:

- `period_middle_day`: the middle day of `original_period` (can be useful when you compare graphically interpolated series and original ones).
- `filtered` (boolean): `TRUE` if the series is filtered, `FALSE` otherwise.

The content of two columns are modified:

- `series_code`: same as before for original series, but the suffix `_filtered` is added for filtered series.
- `series_name`: same as before for original series, but the suffix ` (filtered)` is added for filtered series.

## Transform the `data.table` object into a `xts` object
For some analysis, it is more convenient to have a `xts` object instead of a `data.table` object. To transform
it, you can use the following functions:
```r
library(xts)
library(data.table)
library(rdbnomics)

to_xts <- function(
  x,
  needed_columns = c("period", "series_code", "series_name", "value"),
  series_columns = c("series_code", "series_name")
) {
  if (is.null(x)) {
    return(NULL)
  }

  all_cols <- length(setdiff(needed_columns, colnames(x))) != 0
  if (all_cols) {
    stop(
      paste0(
        "To export as a xts object, some columns are missing. Needed columns ",
        "are \u0022", paste0(needed_columns, collapse = "\u0022, \u0022"),
        "\u0022"
      ),
      call. = FALSE
    )
  }

  x <- x[, .SD, .SDcols = needed_columns]
  data.table::setcolorder(x, needed_columns)

  attr_names <- NULL
  if (!is.null(series_columns)) {
    attr_names <- unique(x[, .SD, .SDcols = series_columns])
  }

  if (nrow(x) > 0) {
    x <- data.table::dcast.data.table(
      x, period ~ series_code,
      value.var = "value"
    )
  } else {
    orig <- Sys.Date() - as.numeric(Sys.Date())
    x <- data.table(
      period = as.Date(numeric(), origin = orig),
      no_code = numeric()
    )
  }

  if (!inherits(x[[1]], "Date")) {
    stop(
      paste0(
        "The first needed column '", needed_columns[1], "' is not of class ",
        "'Date' which is a problem for data.table::as.xts.data.table()."
      ),
      "\n",
      paste0(
        "Please check with packageVersion(\"rdbnomics\") that your version is ",
        "greater or equal to '0.5.2'. Otherwise update rdbnomics or contact ",
        "s915.stem@gmail.com."
      ),
      call. = FALSE
    )
  }

  x <- data.table::as.xts.data.table(x)
  xts::xtsAttributes(x) <- list(codename = attr_names)

  x
}

rdb("IMF", "BOP", mask = "A.FR+ES.BCA_BP6_EUR")
#>      ... original_value     period provider_code REF_AREA Reference Area      series_code ...
#>   1:                 NA 1940-01-01           IMF       ES          Spain A.ES.BCA_BP6_EUR
#>   2:                 NA 1941-01-01           IMF       ES          Spain A.ES.BCA_BP6_EUR
#>  ---                ...        ...           ...      ...            ...              ...                                                                    
#> 159:           -15136.8 2018-01-01           IMF       FR         France A.FR.BCA_BP6_EUR
#> 160:                 NA 2019-01-01           IMF       FR         France A.FR.BCA_BP6_EUR

to_xts(rdb("IMF", "BOP", mask = "A.FR+ES.BCA_BP6_EUR"))
#>            A.ES.BCA_BP6_EUR A.FR.BCA_BP6_EUR
#> 1940-01-01               NA               NA
#> 1941-01-01               NA               NA
#> 1942-01-01               NA               NA
#>        ...              ...              ...
#> 2017-01-01            31086       -16397.700
#> 2018-01-01            23283       -15136.800
#> 2019-01-01               NA               NA
```

In the `xts` object, the series codes are used as column names. If you prefer
the series names (or apply a function to them), you can utilize the function:
```r
library(magrittr)

rdb_rename_xts <- function(x, fun = NULL, ...) {
  nm <- xts::xtsAttributes(x)$codename
  cols <- nm$series_name[match(names(x), nm$series_code)]
  if (is.null(fun)) {
    names(x) <- cols
  } else {
    names(x) <- sapply(X = cols, FUN = fun, ..., USE.NAMES = FALSE)
  }
  x
}

rdb("IMF", "BOP", mask = "A.FR+ES.BCA_BP6_EUR") %>%
  to_xts() %>%
  rdb_rename_xts()
#>            Annual – Spain – Current Account, Total, Net, Euros Annual – France – Current Account, Total, Net, Euros
#> 1940-01-01                                                  NA                                                   NA
#> 1941-01-01                                                  NA                                                   NA
#> 1942-01-01                                                  NA                                                   NA
#>        ...                                                 ...                                                  ...
#> 2017-01-01                                               31086                                           -16397.700
#> 2018-01-01                                               23283                                           -15136.800
#> 2019-01-01                                                  NA                                                   NA

rdb("IMF", "BOP", mask = "A.FR+ES.BCA_BP6_EUR") %>%
  to_xts() %>%
  rdb_rename_xts(stringr::word, start = 3)
#>            Spain     France  
#> 1940-01-01    NA         NA
#> 1941-01-01    NA         NA
#> 1942-01-01    NA         NA
#>        ...   ...        ...
#> 2017-01-01 31086 -16397.700
#> 2018-01-01 23283 -15136.800
#> 2019-01-01    NA         NA
```

## P.S.
Visit <a href="https://db.nomics.world/" target="_blank">https://db.nomics.world/</a> !
