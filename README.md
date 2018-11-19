# rdbnomics

[![pipeline status](https://git.nomics.world/dbnomics/rdbnomics/badges/master/pipeline.svg)](https://git.nomics.world/dbnomics/rdbnomics/commits/master)

## DBnomics R client

This package provides you access to DBnomics data series. DBnomics is an open-source project with the goal of aggregating the world's economic data in one location, free of charge to the public. DBnomics covers hundreds of millions of series from international and national institutions (Eurostat, World Bank, IMF, ...).

To use this package, you have to provide the codes of the provider, dataset and series you want. You can retrieve them directly on the <a href="https://db.nomics.world/" target="_blank">website</a>. You have access to the API through this <a href="http://api.db.nomics.world/" target="_blank">link</a> and the documentation is <a href="https://api.db.nomics.world/apidocs" target="_blank">here</a>.

DBnomics is hosted on its own <a href="https://git.nomics.world/" target="_blank">gitlab platform</a>. However, in order to install the package more easily, we created a mirror of this package on <a href="https://github.com/dbnomics/rdbnomics" target="_blank">github</a>.

To install `rdbnomics` from CRAN :

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
vignette("rdbnomics-tutorial")
```

## Examples
Fetch time series by `ids` :
```r
# Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
# Fetch two series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df2 <- rdb(ids = c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN', 'AMECO/ZUTN/DNK.1.0.0.0.ZUTN'))
# Fetch two series from different datasets of different providers:
df3 <- rdb(ids = c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN', 'IMF/CPI/A.AT.PCPIT_IX'))
```

Fetch time series by `mask` (only for some providers, check the <a href="https://git.nomics.world/dbnomics/dbnomics-api/blob/master/dbnomics_api/application.cfg" target="_blank">list</a>).
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

Fetch time series by `dimensions` :
```r
# Fetch one value of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df1 <- rdb('AMECO', 'ZUTN', dimensions = '{"geo": ["ea12"]}')
# Fetch two values of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df2 <- rdb('AMECO', 'ZUTN', dimensions = '{"geo": ["ea12", "dnk"]}')
# Fetch several values of several dimensions from dataset 'Doing business' (DB) of World Bank:
df3 <- rdb('WB', 'DB', dimensions = '{"country": ["DZ", "PE"],"indicator": ["ENF.CONT.COEN.COST.ZS","IC.REG.COST.PC.FE.ZS"]}')
```

Fetch one series from dataset 'Doing Business' of WB provider:
```r
df1 <- rdb_by_api_link('https://api.db.nomics.world/v21/series?dimensions=%7B%22country%22%3A%5B%22FR%22%2C%22IT%22%2C%22ES%22%5D%2C%22indicator%22%3A%5B%22IC.REG.PROC.FE.NO%22%5D%7D&provider_code=WB&dataset_code=DB&format=json')
```

## Connection error `Could not resolve host`
When using the functions `rdb` or `rdb_by_api_link`, you may come across the following error:
```r
Error in open.connection(con, "rb") :
  Could not resolve host: api.db.nomics.world
```
This error is due to a fail of the function `fromJSON`. To get round this situation, you can use the `readLines` function before the `fromJSON` function by setting an option of the package:
```r
options(rdbnomics.use_readLines = TRUE)

df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN')

df2 <- rdb(ids = c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN', 'AMECO/ZUTN/DNK.1.0.0.0.ZUTN'))
```
or by using the argument `use_readLines` of the function `rdb_by_api_link`:
```r
df1 <- rdb(ids = 'AMECO/ZUTN/EA19.1.0.0.0.ZUTN', use_readLines = TRUE)

df2 <- rdb(ids = c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN', 'AMECO/ZUTN/DNK.1.0.0.0.ZUTN'), use_readLines = TRUE)
```
