# DBnomics R client

This package provides you access to DBnomics data series. DBnomics is an open-source project with the goal of aggregating the world’s economic data in one location, free of charge to the public. DBnomics covers hundreds of millions of series from international and national institutions (Eurostat, World Bank, IMF, ...).

To use this package, you have to provide the codes of the provider, dataset and series you want. You can retrieve them directly on the [website](https://next.nomics.world/). You have access to the API through this [link](http://api.next.nomics.world/) and the documentation is [**here**](https://api.next.nomics.world/apidocs).

DBnomics is hosted on its own [Gitlab platform](https://git.nomics.world/). However, in order to install the package more easily, we created a mirror of this package on GitHub.

To install `rdbnomics`, you will need **devtools** :

```r
devtools::install_github("dbnomics/rdbnomics")
```

3 ways to fetch the same series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:

```r
# Series codes
df1    <- rdb_by_codes('AMECO','ZUTN','EA19.1.0.0.0.ZUTN')
# Dimensions
df1bis <- rdb_by_dimensions('AMECO','ZUTN','{"geo": ["ea12"]}')
# Url
df1ter <- rdb_by_url('https://api.next.nomics.world/AMECO/ZUTN?series_codes=EA19.1.0.0.0.ZUTN')
```

3 ways to fetch two series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:

```r
# Series codes
df2    <- rdb_by_codes('AMECO','ZUTN',c('EA19.1.0.0.0.ZUTN','DNK.1.0.0.0.ZUTN'))
# Dimensions
df2bis <- rdb_by_dimensions('AMECO','ZUTN','{"geo": ["ea12", "dnk"]}')
# Url
df2ter <- rdb_by_url('https://api.next.nomics.world/AMECO/ZUTN?series_codes=EA19.1.0.0.0.ZUTN,DNK.1.0.0.0.ZUTN')
```

Fetch several values of several dimensions from dataset 'Doing business' (DB) of World bank:

```r
dim <- '{"country": ["DZ", "BT", "PE"],
         "indicator": ["IC.DCP.BQCI","IC.REG.COST.PC.ZS"]}'
df3 <- rdb_by_dimensions('world-bank','DB',dim)
```

Fetch one dataset 'Exports and imports by Member States of the EU/third countries' (namq_10_exi) of Eurostat provider:

```r
df3 <- rdb_by_codes('Eurostat','namq_10_exi')
```




