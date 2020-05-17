# coviDC

## Maps of SARS-CoV-2 Data in the District of Columbia (2020) Using R 
 
Open-source code to download, manage, and visualize publicly available COVID-19 epidemiologic data in the District of Columbia. See this [blog post](https://idblr.rbind.io/post/2020-05-15-covid-dc/covid-dc/) for an example of maps for May 15, 2020.

### Data Sources

1. DC Mayoral Office [coronavirus data](https://coronavirus.dc.gov/page/coronavirus-data)
2. Collated daily cumulative case count and case rates (per 1,000) by Molly Tolzmann [zmotoly](https://twitter.com/zmotoly) available on a public [Google Sheet]("https://docs.google.com/spreadsheets/d/1u-FlJe2B1rYV0obEosHBks9utkU30-C2TSkHka6AVS8/edit#gid=1923705378").
3. Presented at the [DC health planning neighborhood level](https://opendata.dc.gov/datasets/dc-health-planning-neighborhoods)

#### Important Notes
* Case data does not account for degree of testing across DC
* Case rate based on the 2018 American Community Survey (ACS) census tract data available from [OpenData DC](https://opendata.dc.gov/datasets/acs-2018-population-variables-tract) at the Census Tract level.
* Case data are cumulative since beginning of the COVID-19 outbreak in DC.

### Getting Started

The R workflow was built with `R version 3.6.3 (2020-02-29) -- "Holding the Windsock"` and requires several established `R` packages. 

```
  loadedPackages <- c("broom", "geojsonio", "ggplot2", "googlesheets4", "htmlwidgets", "leaflet", "sp", "stringr")
  invisible(lapply(loadedPackages, require, character.only = T))
```

Follow the [map_covid_dc.R](https://github.com/idblr/coviDC/blob/master/code/map_covid_dc.R) file to generate the example maps from May 15, 2020.

### In Development

We enough temporal data, the change in COVID-19 cases per 1,000 would be possible to estimate across DC and may indicate recent upticks in cases. Stay tuned!

### Authors

* **Ian Buller** - *Initial heavycoding with expertise in spatial statistics* - [GitHub](https://github.com/idblr)

See also the list of [contributors](https://github.com/idblr/coviDC/graphs/contributors) who participated in this project.

### License

This project is licensed under the Apache 2.0 License - see the [LICENSE.md](https://github.com/idblr/coviDC/blob/master/LICENSE) file for details

### Acknowledgments

* Huge s/o to Molly Tolzmann [zmotoly](https://twitter.com/zmotoly) for the data collation and initial inspiration from [her original post](https://www.popville.com/2020/05/dc-neighborhood-covid-coronavirus-map-population/#more-234053) on [PoPville](https://twitter.com/PoPville).
* Big thanks to the team at [Open Data DC](https://opendata.dc.gov/)
* And to the DC Mayoral Office for the publicly avaiable [coronavirus data](https://coronavirus.dc.gov/page/coronavirus-data) and lcoal leadership