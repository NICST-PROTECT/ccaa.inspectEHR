
<!-- README.md is generated from README.Rmd. Please edit that file -->

# d.inspectEHR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

This package uses Ed’s d.inspectEHR package to produce the data quality
and summary reports on OMOP. Prior to running these reports, sites
*must* apply the DECOVID ddls and *should* try to run ACHILLES against
their OMOP database.

## Using the package as a user

### Installation

You can install the development version of ccaa.inspectEHR from
[GitHub](https://github.com/DocEd/d.inspectEHR) with:

``` r
remotes::install_github("DocEd/d.inspectEHR")
```

To run the reports as an end user, install from github first (will only
work if you have access to the repo). If you don’t have a personal
access token, generate it using the instructions here.
<https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token>

Then run the function below using the token generated.

``` r
devtools::install_github("NICST-PROTECT/ccaa.inspectEHR", auth_token = "your_PAT")
```

### Running the Report

The report can be run by running the following code.

- The date range that you need to generate the report should be give
  with *start_date* and *end_date*.
- The acceptable tolerance for measurement checks can be given using the
  *measurement_tolerance*

``` r
library(d.inspectEHR)

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
    start_date = '01-01-2020',
    end_date = '31-10-2022'
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
    start_date = '01-01-2020',
    end_date = '31-10-2022',
    measurement_tolerance = '0.01'
    
  )
)
```

## Run as a developer

If you intend to edit code, clone the repo, double click the .Rproj file
to open, and use devtools::load_all() to make the functions available in
the environment. Follow the instructions above to actually knit the
reports. General information on package development here.
<https://r-pkgs.org>

### Rules for contributing

- Create a new branch and send Aasiyah a pull request review.
- Also share sample generated PDFs.
- Never add generated PDFs to this repo, because it increases the
  package size.
- Use roxygen documentation for any functions you create. If it’s not
  meant to be accessible to users, add the @noRd tag.
