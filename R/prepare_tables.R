#' Get patients for a selected time period and return as a list
#' Beased on this list all the tables will be filtered
#' 
#' @param df the dataframe which patients selects from
#' @param start_date begining of the period
#' @param end_date end of the period
#' 
#' @return 
#' @export
#' 

get_period_patients <- function(df, start_date, end_date){
  start_date =  as.Date(start_date, format= '%d-%m-%Y')
  end_date = as.Date(end_date, format= '%d-%m-%Y')
  patients <- df[(df$observation_period_start_date >= start_date) & (df$observation_period_end_date<=end_date),] %>% select('person_id')
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
#' @export
prepare_tables <- function(connection, schema_name) {

  # Reading the tables in.
  tables <- c("person", "visit_occurrence", "death")

  st <- 
    map(tables, ~collect(tbl(connection, in_schema(schema_name, .x)))) %>% 
    setNames(tables)
  
  # Translating the concept IDs to names.
  all_concepts <- st %>%
    map(~ select(., contains("concept_id")) %>%
          pivot_longer(everything(),
                       names_to = "column_name",
                       values_to = "concept_id")) %>%
    bind_rows() %>%
    distinct(.data$concept_id, .keep_all = TRUE)

  replace_names <- mini_dict(connection, schema_name, all_concepts$concept_id)

  st %>%
    map(~
          mutate(
            .,
            across(where(is.integer64), as.integer))) %>%
    map(~
          mutate(
            .,
            across(
              c(contains("concept_id"),
                -contains("source"),
                contains("admitted_from_concept_id")),
              match_concepts, lookup = replace_names))
    ) %>%
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
#' @importFrom rlang .data
#' @importFrom lubridate interval years
#'
#' @return
#' @export
prepare_overview <- function(x) {

  x[["person"]] %>%
    select(.data$person_id,
           .data$gender_concept_id,
           .data$year_of_birth) %>%
    left_join(x[["death"]] %>%
                select(.data$person_id,
                       .data$death_date,
                       .data$death_datetime),
              by = "person_id") %>%
    left_join(x[["visit_occurrence"]] %>%
                select(.data$person_id,
                       .data$visit_occurrence_id,
                       .data$visit_start_datetime,
                       .data$visit_end_datetime,
                       .data$visit_concept_id,
                       .data$admitted_from_concept_id,
                       .data$discharged_to_concept_id),
              by = "person_id") %>%
    # As per OMOP guidance, missing dates of birth are imputed as June 15th.
    mutate(
      age = interval(start = as.Date(paste0(.data$year_of_birth, "-06-15")),
                     end = .data$visit_start_datetime)/years(1))
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
#' @export
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
                                        "observation")) {

  clinical_tbls <- tibble(table = tbl_names)

  ttally <- clinical_tbls$table %>%
    map_int(~ tbl(ctn, in_schema(schema, .)) %>%
              tally() %>%
              collect() %>%
              pull() %>%
              as.integer())

  clinical_tbls %>%
    mutate(n = ttally)
}


#' Prepare NULL count information
#'
#' @param st a prepped table list from \code{\link{prepare_tables}}
#'
#' @return
#' @export
#'
#' @importFrom purrr imap
#' @importFrom dplyr bind_rows filter left_join
#'
#' @examples
prepare_null_counts <- function(st) {

  null_counts <- st %>%
    imap(~ summarise_missing(.x, .y)) %>%
    bind_rows() %>%
    filter(!grepl(x = column, pattern = "source"))

  left_join(null_counts, core_null_tolerance,
            by = c("table", "column"))

}









