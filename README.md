
# ccaa.inspectEHR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The aim of ccaa.inspectEHR is to provide a high-level profile, and data quality evaluation of a critical care database in OMOP format. The package produces 2 html reports, one describing the data available, and one applying a set of data quality checks based on the [Khan framework](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5051581/).

The package is based on a [simillar quality evaluation](https://github.com/DocEd/d.inspectEHR) for the DeCovid project. 

Users must have an SQL database conforming to the OMOP v5.4 specifications. 

### Installation

You can install the development version of ccaa.inspectEHR from
[GitHub](https://github.com/NICST-PROTECT/ccaa.inspectEHR) with:

``` r
remotes::install_github("NICST-PROTECT/ccaa.inspectEHR")
```

### Running the Report

The report can be run by running the following code.

- The date range that you need to generate the report should be give
  with *start_date* and *end_date*.
- The acceptable tolerance for measurement checks can be given using the
  *measurement_tolerance*

``` r
library(ccaa.inspectEHR)

render_summary_report(
  output = "Summary_report.html",
  params = list(
    driver = "PostgreSQL",
    host = "localhost",
    port = 5432,
    dbname = "cca_omop",
    schema = "cca_omop",
    user = #####,
    password = #####,
    local_hospital = "ccaa",
    start_date = '2020-01-01',
    end_date = '2022-10-31',
    custom_colors = c("#379481","#4CA1CB","#3AB8EE","#569FB2")
  )
)

render_quality_report(
  output = "Data_Quality_report.html",
  params = list(
    driver = "PostgreSQL",
    host = "localhost",
    port = 5432,
    dbname = "cca_omop",
    schema = "cca_omop",
    user = ######,
    password = ######,
    local_hospital = "ccaa",
    start_date = '2020-01-01',
    end_date = '2022-10-31',
    measurement_tolerance = '0.01'
    
  )
)
```
