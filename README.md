# DBnomics R client

This package provides you access to DBnomics data series. DBnomics is an open-source project with the goal of aggregating the world’s economic data in one location, free of charge to the public. DBnomics covers hundreds of millions of series from international and national institutions (Eurostat, World Bank, IMF, ...).

To use this package, you have to provide the codes of the provider, dataset and series you want. You can retrieve them directly on the [website](https://next.nomics.world/). You have access to the API through this [link](http://api.next.nomics.world/) and the documentation is [here](https://api.next.nomics.world/apidocs).

DBnomics is hosted on its own [Gitlab platform](https://git.nomics.world/). However, in order to install the package more easily, we created a mirror of this package on GitHub.

To install `rdbnomics`, you will need `devtools`:

```r
devtools::install_github("dbnomics/rdbnomics")
library(rdbnomics)
```

Fetch series by id :
```r
# Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df1 <- rdb(ids='AMECO/ZUTN/EA19.1.0.0.0.ZUTN')
# Fetch two series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df2 <- rdb(ids=c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN','AMECO/ZUTN/DNK.1.0.0.0.ZUTN'))
# Fetch two series from different datasets of different providers:
df3 <- rdb(ids=c('AMECO/ZUTN/EA19.1.0.0.0.ZUTN','IMF/CPI/A.AT.PCPIT_IX'))
```

Fetch series by dimension :
```r
# Fetch one value of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df1 <- rdb('AMECO','ZUTN',dimensions='{"geo": ["ea12"]}')
# Fetch two values of one dimension from dataset 'Unemployment rate' (ZUTN) of AMECO provider:
df2 <- rdb('AMECO','ZUTN',dimensions='{"geo": ["ea12", "dnk"]}')
# Fetch several values of several dimensions from dataset 'Doing business' (DB) of World Bank:
df3 <- rdb('WB','DB',dimensions='{"country": ["DZ", "BT", "PE"],"indicator": ["IC.DCP.BQCI","IC.REG.COST.PC.ZS"]}')
```

Fetch series by mask (only for some providers, check the [list](https://git.nomics.world/dbnomics/dbnomics-api/blob/master/dbnomics_api/application.cfg)).
```r
# Fetch one series from dataset 'Consumer Price Index' (CPI) of IMF:
df1 <- rdb('IMF','CPI',mask='M.DE.PCPIEC_WT')
# Fetch two series from dataset 'Consumer Price Index' (CPI) of IMF:
df2 <- rdb('IMF','CPI',mask='M.DE+FR.PCPIEC_WT')
# Fetch all series along one dimension from dataset 'Consumer Price Index' (CPI) of IMF:
df3 <- rdb('IMF','CPI',mask='M..PCPIEC_WT')
# Fetch series along multiple dimensions from dataset 'Consumer Price Index' (CPI) of IMF:
df4 <- rdb('IMF','CPI',mask='M..PCPIEC_IX+PCPIA_IX')
```
