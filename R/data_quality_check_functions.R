#' check duplicates of a given data frame column
#'
#' @param dataset an dataframe
#' @param column the column interested within ''
#'
#' @return
#'
find_duplicates <- function(dataset, column) {
  dataset <- data.frame(dataset)
  duplicates <- dataset[duplicated(dataset[column]), ]
  row.names(duplicates) <- NULL
  return(duplicates)
}


#' check one column values appear in another column at least once
#' in a given data frame column
#'
#' @param check_df an dataframe we performe check
#' @param check_col the column interested within dataset_1
#' @param compare_df
#' @param compare_col
#'
#' @importFrom magrittr `%>%`
#'
#' @return
#'

check_id_availability <- function(check_df, check_col, compare_df, compare_col) {
  unavailable <- check_df[!check_df[[check_col]] %in% compare_df[[compare_col]], ]
  row.names(unavailable) <- NULL
  return(unavailable)
}


#' check whether a date is before or after the given date from a dataframe
#'
#' @param check_df an dataframe
#' @param check_col the column interested within dataset for checking
#' @param compare_df optional. if the comapring date comes from a dataframe
#' @param compare_date comparing date or column in a dataframe
#' @param check_arg one of the ==, >=,>,<=,<
#'
#' @importFrom magrittr `%>%`
#'
#' @return
#'

check_date_within <- function(check_df, check_date, compare_df, compare_date, check_arg) {
  if (missing(compare_df)) {
    compare_df <- data.frame(matrix(ncol = 0, nrow = nrow(check_df)))
    compare_df$compare <- as.Date(compare_date)
    compare_df$person_id <- check_df$person_id
  } else {
    compare_df$compare <- compare_df[[eval(compare_date)]]
  }

  check_df$check <- check_df[[eval(check_date)]]

  df <- check_df %>%
    select(person_id, check) %>%
    left_join(
      compare_df %>%
        select(person_id, compare),
      by = "person_id"
    ) %>%
    filter(eval(parse(text = paste("check", check_arg, "compare"))))

  return(df)
}



#' This function will give the concept_id of a given source field in the mapping sheet
#'
#' @param measure_field the source or measure field that interested on
#' @param mapping_sheet the mapping sheet which have the source field and concept id mappings
#'
#' @return

get_source_concept_id <- function(measure_field, mapping_sheet) {
  concept_id <- mapping_sheet %>%
    filter(source_field == measure_field) %>%
    pull(concept_id) %>%
    unique()
  return(concept_id)
}

#' This function will check whether the given measurement is within the acceptable rate
#'
#' @param measurement the measurement variable that need to check
#' @param bound_df the data table which have all the measurement bounds
#' @param check_df the dataframe
#'
#' @return

check_measure_bounds <- function(measurement, bound_df, check_df) {
  lower_bound <- bound_df %>%
    filter(Variable == measurement) %>%
    pull(`Lower bound`)
  upper_bound <- bound_df %>%
    filter(Variable == measurement) %>%
    pull(`Upper bound`)

  abnormal_df <- check_df %>% filter(value_as_number < lower_bound | value_as_number > upper_bound)
  return(abnormal_df)
}


#' This function will add data quality queries and thair status in to a one dataframe
#'
#' @param qual_df  the data frame where to add data quality query and status
#' @param Total the denominator of the check
#' @param check the check/query
#' @param status Pass/Fail status
#' @param fail_count fail count, if check pass this is zero
#' @param fail_percentage fail percentage, if check pass this is zero
#'
#' @return

add_new_check <- function(qual_df, Total, check, status, fail_count, fail_percentage) {
  new_row <- c(check, Total, status, paste(fail_count, paste("(", fail_percentage, "%)")))
  qual_df[nrow(qual_df) + 1, ] <- new_row
  return(qual_df)
}
