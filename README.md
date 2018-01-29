This package provides you access to DBnomics data series. DBnomics is an open-source project with the goal of aggregating the world’s economic data in one location, free of charge to the public. DBnomics covers hundreds of millions of series from international and national institutions (Eurostat, World Bank, IMF, ...).

To use this package, you have to provide the codes of the provider, dataset and series you want. You can retrieve them directly on the [website](https://next.nomics.world/). You have access to the API through this [link](http://api.next.nomics.world/) and the documentation is [**here**](https://api.next.nomics.world/apidocs).

DBnomics is hosted on its own [Gitlab platform](https://git.nomics.world/). However, in order to install the package more easily, we created a mirror of this package on GitHub.

To install `rdbnomics`, you will need **devtools** :

```r
devtools::install_github("dbnomics/rdbnomics")
```

Fetch one series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:

```r
df1 <- rdb('AMECO','ZUTN','EA19.1.0.0.0.ZUTN')
```

Fetch two series from dataset 'Unemployment rate' (ZUTN) of AMECO provider:

```r
df2 <- rdb('AMECO','ZUTN',c('EA19.1.0.0.0.ZUTN','DNK.1.0.0.0.ZUTN'))
```

Fetch one dataset 'Exports and imports by Member States of the EU/third countries' (namq_10_exi) of Eurostat provider:

```r
df3 <- rdb('Eurostat','namq_10_exi')
```




