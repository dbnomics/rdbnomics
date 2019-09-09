# rdbnomics <img src="man/figures/logo.png" align="right" width="120" />

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rdbnomics)](https://cran.r-project.org/package=rdbnomics)
[![pipeline status](https://git.nomics.world/dbnomics/rdbnomics/badges/master/pipeline.svg)](https://git.nomics.world/dbnomics/rdbnomics/commits/master)
[![status](https://tinyverse.netlify.com/badge/rdbnomics)](https://CRAN.R-project.org/package=rdbnomics)

## DBnomics R client

This package provides you access to DBnomics data series. DBnomics is an open-source project with the goal of aggregating the world's economic data in one location, free of charge to the public. DBnomics covers hundreds of millions of series from international and national institutions (Eurostat, World Bank, IMF, ...).

To use this package, you have to provide the codes of the provider, dataset and series you want. You can retrieve them directly on the <a href="https://db.nomics.world/" target="_blank">website</a>. You have access to the API through this <a href="http://api.db.nomics.world/" target="_blank">link</a> and the documentation is <a href="https://api.db.nomics.world/apidocs" target="_blank">here</a>.

DBnomics is hosted on its own <a href="https://git.nomics.world/" target="_blank">gitlab platform</a>. However, in order to install the package more easily, we created a mirror of this package on <a href="https://github.com/dbnomics/rdbnomics" target="_blank">github</a>.

To install `rdbnomics` from CRAN:

```r
install.packages("rdbnomics")
library(rdbnomics)
```

To install `rdbnomics` from github:

```r
remotes::install_github("dbnomics/rdbnomics", build = TRUE, build_opts = c("--no-resave-data", "--no-manual"), force = TRUE)
library(rdbnomics)
```

After installation, a vignette is available to the user:
```r
vignette("rdbnomics")
```

## Examples
Fetch time series by `ids`:
```r
# Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN')

# Fetch two series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df2 <- rdb(ids = c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN', 'AMECO/ZUTN/DNK.1.0.0.0.ZUTN'))

# Fetch two series from different datasets of different providers:
df3 <- rdb(ids = c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN', 'IMF/CPI/A.AT.PCPIT_IX'))
```

In the event that you only use the argument `ids`, you can drop it and run:
```r
df <- rdb('AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
```

Fetch time series by `mask` :
```r
# Fetch one series from dataset 'Consumer Price Index' (CPI) of IMF:
df1 <- rdb('IMF', 'CPI', mask = 'M.DE.PCPIEC_WT')

# Fetch two series from dataset 'Consumer Price Index' (CPI) of IMF:
df2 <- rdb('IMF', 'CPI', mask = 'M.DE+FR.PCPIEC_WT')

# Fetch all series along one dimension from dataset 'Consumer Price Index' (CPI) of IMF:
df3 <- rdb('IMF', 'CPI', mask = 'M..PCPIEC_WT')

# Fetch series along multiple dimensions from dataset 'Consumer Price Index' (CPI) of IMF:
df4 <- rdb('IMF', 'CPI', mask = 'M..PCPIEC_IX+PCPIA_IX')
```

In the event that you only use the arguments `provider_code`, `dataset_code` and `mask`, you can drop the name `mask` and run:
```r
df <- rdb('IMF', 'CPI', 'M.DE.PCPIEC_WT')
```

Fetch time series by `dimensions`:
```r
# Fetch one value of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df1 <- rdb('AMECO', 'ZUTN', dimensions = list(geo = "ea12"))
# or
df1 <- rdb('AMECO', 'ZUTN', dimensions = '{"geo": ["ea12"]}')

# Fetch two values of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df2 <- rdb('AMECO', 'ZUTN', dimensions = list(geo = c("ea12", "dnk")))
# or
df2 <- rdb('AMECO', 'ZUTN', dimensions = '{"geo": ["ea12", "dnk"]}')

# Fetch several values of several dimensions from dataset 'Doing business' (DB) of World Bank:
df3 <- rdb('WB', 'DB', dimensions = list(country = c("DZ", "PE"), indicator = c("ENF.CONT.COEN.COST.ZS", "IC.REG.COST.PC.FE.ZS")))
# or
df3 <- rdb('WB', 'DB', dimensions = '{"country": ["DZ", "PE"], "indicator": ["ENF.CONT.COEN.COST.ZS", "IC.REG.COST.PC.FE.ZS"]}')
```

Fetch one series from the dataset 'Doing Business' of WB provider with the link:
```r
df1 <- rdb_by_api_link('https://api.db.nomics.world/v22/series/WB/DB?dimensions=%7B%22country%22%3A%5B%22FR%22%2C%22IT%22%2C%22ES%22%5D%7D&q=IC.REG.PROC.FE.NO&observations=1&format=json&align_periods=1&offset=0&facets=0')
```

## Proxy configuration or connection error `Could not resolve host`
When using the functions `rdb` or `rdb_...`, you may come across the following error:
```r
Error in open.connection(con, "rb") :
  Could not resolve host: api.db.nomics.world
```
To get round this situation, you have two possibilities:

1. configure **curl** to use a specific and authorized proxy.

2. use the default R internet connection i.e. the Internet Explorer proxy defined in *internet2.dll*.

### Configure **curl** to use a specific and authorized proxy
In **rdbnomics**, by default the function `curl_fetch_memory` (of the package **curl**) is used to fetch the data. If a specific proxy must be used, it is possible to define it permanently with the package option `rdbnomics.curl_config` or on the fly through the argument `curl_config`. In that way:

- if the object is of class `curl_handle` then it is passed to the argument `handle` of the `curl_fetch_memory` function,
- if the object is a named list then the elements are passed to the connection (the `curl_handle` object created internally with `new_handle()`) with `handle_setopt()` before using `curl_fetch_memory`.

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
df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
```
This option of the package can be disabled with:
```r
options(rdbnomics.curl = NULL)
```

#### Use the connection only for a function call
If a complete configuration is not needed but just an "on the fly" execution, then use the argument `curl_config` of the functions `rdb` and `rdb_...`:
```r
df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN', curl_config = h)
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
df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
```
This configuration can be disabled with:
```r
options(rdbnomics.use_readLines = FALSE)
```

#### Use the connection only for a function call
If you just want to do it once, you may use the argument `use_readLines` of the functions `rdb` and `rdb_...`:
```r
df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN', use_readLines = TRUE)
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
  x <- data.table::as.xts.data.table(x)
  xts::xtsAttributes(x) <- list(codename = attr_names)

  x
}

rdb("IMF", "CPI", mask = "M.DE+FR.PCPIEC_WT")
#>      @frequency dataset_code               dataset_name          indexed_at original_period     period
#>   1:    monthly          CPI Consumer Price Index (CPI) 2019-05-19 08:19:35         1996-01 1996-01-01
#>   2:    monthly          CPI Consumer Price Index (CPI) 2019-05-19 08:19:35         1996-02 1996-02-01
#>             ...          ...                        ...                 ...             ...        ...
#> 569:    monthly          CPI Consumer Price Index (CPI) 2019-05-19 08:19:35         2019-02 2019-02-01
#> 570:    monthly          CPI Consumer Price Index (CPI) 2019-05-19 08:19:35         2019-03 2019-03-01

to_xts(rdb("IMF", "CPI", mask = "M.DE+FR.PCPIEC_WT"))
#>            M.DE.PCPIEC_WT M.FR.PCPIEC_WT
#> 1995-01-01             NA           20.0
#> 1995-02-01             NA           20.0
#>        ...            ...            ...
#> 2019-02-01          30.10           25.8
#> 2019-03-01          30.10           25.8
```

In the `xts` object, the series codes are used as column names. If you prefer
the series names (or apply a function to them), you can utilize the function:
```r
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

library(magrittr)

rdb("IMF", "CPI", mask = "M.DE+FR.PCPIEC_WT") %>%
  to_xts() %>%
  rdb_rename_xts()
#>            Monthly – Germany – Communication, Weight Monthly – France – Communication, Weight
#> 1995-01-01                                        NA                                     20.0
#> 1995-02-01                                        NA                                     20.0
#>        ...                                       ...                                      ...
#> 2019-02-01                                     30.10                                     25.8
#> 2019-03-01                                     30.10                                     25.8


rdb("IMF", "CPI", mask = "M.DE+FR.PCPIEC_WT") %>%
  to_xts() %>%
  rdb_rename_xts(stringr::word, start = 3)
#>            Germany France
#> 1995-01-01      NA   20.0
#> 1995-02-01      NA   20.0
#>        ...     ...    ...
#> 2019-02-01   30.10   25.8
#> 2019-03-01   30.10   25.8
```

## P.S.
Visit <a href="https://db.nomics.world/" target="_blank">https://db.nomics.world/</a> !
