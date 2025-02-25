#' Get patients for a selected time period and return as a list
#' Based on this list all the tables will be filtered
#' to get the period patients, the observation period table is used
#'
#' @param df the dataframe which patients selects from
#' @param start_date begining of the period
#' @param end_date end of the period
#'
#' @return
#'

get_period_patients <- function(ctn, schema, start_date, end_date) {
  df <- tbl(ctn, in_schema(schema, "observation_period")) %>% as_tibble()
  start_date <- as.Date(start_date, format = "%Y-%m-%d")
  end_date <- as.Date(end_date, format = "%Y-%m-%d")
  patients <- df[(df$observation_period_start_date >= start_date) & (df$observation_period_end_date <= end_date), ] %>% select("person_id")
  patients$person_id
}


#' Prepare Small Database Tables
#'
#' Collects and processes some of the smaller database tables:
#'
#'   - person
#'   - visit_occurrence
#'   - visit_detail
#'   - death
#'
#' As these tables grow, it might be prudent to do this preparation in the DB.
#'
#' @param connection a connection object created with \code{\link{DBI::dbConnect}}
#' @param schema_name a character vector for the target schema within the DB
#'
#' @importFrom dplyr collect tbl select across bind_rows distinct filter mutate
#' @importFrom dbplyr in_schema
#' @importFrom purrr map
#' @importFrom rlang !! .data
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect contains everything
#'
#' @return
prepare_tables <- function(connection, schema_name) {
  # Reading the tables in.
  tables <- c("person", "visit_occurrence", "death")

  st <-
    map(tables, ~ collect(tbl(connection, in_schema(schema_name, .x)))) %>%
    setNames(tables)

  # Translating the concept IDs to names.
  all_concepts <- st %>%
    map(~ select(., contains("concept_id")) %>%
          pivot_longer(everything(),
                       names_to = "column_name",
                       values_to = "concept_id"
          )) %>%
    bind_rows() %>%
    distinct(concept_id, .keep_all = TRUE)

  replace_names <- mini_dict(connection, schema_name, all_concepts$concept_id)

  st %>%
    map(~
          mutate(
            .,
            across(where(is.integer64), as.integer)
          )) %>%
    map(~
          mutate(
            .,
            across(
              c(contains("concept_id") & 
                  # Have to exclude most thing with the word source, but one of them is need.
                  (!contains("source") | contains("admitting_source_concept_id"))),
              match_concepts,
              lookup = replace_names
            )
          )) %>%
    map(~
          mutate(
            .,
            across(
              c(contains("date"), -contains("datetime")),
              as.Date
            )
          ))
}

#' Prepare Overview Table
#'
#' @param x the output from \code{\link{prepare_tables}}
#'
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select left_join
#' @importFrom tidyselect any_of
#' @importFrom rlang .data
#' @importFrom lubridate interval years
#'
#' @return
prepare_overview <- function(x) {
  x[["person"]] %>%
    select(person_id, 
           gender_concept_id,
           year_of_birth
    ) %>%
    left_join(
      x[["death"]] %>%
        select(
          person_id,
          death_date,
          death_datetime
        ),
      by = "person_id"
    ) %>%
    left_join(
      x[["visit_occurrence"]] %>%
        select(person_id,
               visit_occurrence_id,
               visit_start_datetime,
               visit_end_datetime,
               visit_concept_id,
               any_of(c("admitted_from_concept_id", "admitting_source_concept_id")),
               any_of(c("discharged_to_concept_id", "discharge_to_concept_id"))
        ),
      by = "person_id"
    ) %>%
    # As per OMOP guidance, missing dates of birth are imputed as June 15th.
    mutate(
      age = interval(
        start = as.Date(paste0(year_of_birth, "-06-15")),
        end = visit_start_datetime
      ) / years(1)
    )
}


#' Prepare Tally of Clinical Tables
#'
#' @param ctn database connection
#' @param schema character vector of target schema
#'
#' @importFrom tibble tibble
#' @importFrom purrr map_int
#' @importFrom dplyr tbl tally collect pull mutate
#' @importFrom dbplyr in_schema
#' @importFrom magrittr %>%
#'
#' @return
#'
#' @examples
prepare_tally <- function(ctn, schema,
                          tbl_names = c(
                            "person",
                            "care_site",
                            "death",
                            "visit_occurrence",
                            "procedure_occurrence",
                            "drug_exposure",
                            "condition_occurrence",
                            "measurement",
                            "observation"
                          ), filter_person_ids, filter_care_sites) {
  clinical_tbls <- tibble(table = tbl_names)

  ttally <- clinical_tbls$table %>%
    map_int(~ {
      tbl_name <- .
      table <- tbl(ctn, in_schema(schema, tbl_name))

      if ("person_id" %in% colnames(table)) {
        table <- table %>% filter(person_id %in% filter_person_ids)
      } else if ("care_site_id" %in% colnames(table)) {
        table <- table %>% filter(care_site_id %in% filter_care_sites)
      }

      tally(table) %>%
        collect() %>%
        pull() %>%
        as.integer()
    })

  clinical_tbls %>%
    mutate(n = ttally)
}


#' add concept names for a given table
#'
#' @param df the data frame we need to rename concept ids to names
#' @param connection database connection
#' @param schema_name character vector of target schema

get_concept_names <- function(df, connection, schema_name) {
  # Translating the concept IDs to names.
  all_concepts <- df %>%
    select(., contains("concept_id")) %>%
    pivot_longer(everything(),
                 names_to = "column_name",
                 values_to = "concept_id"
    ) %>%
    distinct(concept_id, .keep_all = TRUE)

  replace_names <- mini_dict(connection, schema_name, all_concepts$concept_id)

  df <- df %>%
    mutate(., across(where(is.integer64), as.integer)) %>%
    mutate(., across(
      c(contains("concept_id") &
          (!contains("source") | contains("admitting_source_concept_id"))
      ),
      match_concepts,
      lookup = replace_names
    )) %>%
    mutate(., across(
      c(contains("date"), -contains("datetime")),
      as.Date
    ))
  return(df)
}
