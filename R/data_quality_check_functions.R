#' check whether the column is non empty
#' @param dataset an dataframe
#' @param column the column interested within ''
#'
#' @return
#'
check_empty <- function(dataset, column, qual_df, check) {
  
  # check qual_df is exists, otherwise create it
  if (!(exists("qual_df") & is.data.frame(qual_df))) {
    qual_df <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(qual_df) <- c("Check", "Number of entries", "Status", "If check fails : n(%)")
  }
  
  total = nrow(dataset)
  
  # data set is filtered according to the fail condition
  dataset <- dataset %>% 
    filter(is.na(!!sym(column))) 
  
  check_pass_status = ((dataset %>% nrow()) == 0)
  fail_count = nrow(dataset)
  
  qual_df = make_data_quality_df(qual_df,check, check_pass_status,total,fail_count)
  
  qual_df %>%
    print_large_kable() %>%
    column_spec(1, width = "55%") %>%
    column_spec(2:ncol(glossary), width = "15%")
  qual_df
}


#' check a number is within a given range
#'
#' @param dataset an dataframe
#' @param column the column interested within ''
#'
#' @return
#'
check_number_within <- function(dataset, column, min_val, max_val, qual_df, check) {
  
  # check qual_df is exists, otherwise create it
  if (!(exists("qual_df") & is.data.frame(qual_df))) {
    qual_df <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(qual_df) <- c("Check", "Number of entries", "Status", "If check fails : n(%)")
  }

  total = nrow(dataset)
  
  dataset[[column]] <- as.numeric(dataset[[column]])
  dataset <- data.frame(dataset) 
  
  # data set is filtered according to the fail condition
  dataset <- dataset %>% 
    filter(!!sym(column) < min_val | !!sym(column) > max_val) 
  
  check_pass_status = ((dataset %>% nrow()) == 0)
  fail_count = nrow(dataset)
  
  qual_df = make_data_quality_df(qual_df,check, check_pass_status,total,fail_count)
  
  qual_df %>%
    print_large_kable() %>%
    column_spec(1, width = "55%") %>%
    column_spec(2:ncol(glossary), width = "15%")
  qual_df
}


#' check duplicates of a given data frame column
#'
#' @param dataset an dataframe
#' @param column the column interested within ''
#'
#' @return
#'
find_duplicates <- function(dataset, column, qual_df, check) {
  
  # check qual_df is exists, otherwise create it
  if (!(exists("qual_df") & is.data.frame(qual_df))) {
    qual_df <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(qual_df) <- c("Check", "Number of entries", "Status", "If check fails : n(%)")
  }
  
  total = nrow(dataset)
  
  dataset <- data.frame(dataset)
  duplicates <- dataset[duplicated(dataset[column]), ]
  
  check_pass_status = ((duplicates %>% nrow()) ==0)
  fail_count = nrow(duplicates)
  
  qual_df = make_data_quality_df(qual_df,check, check_pass_status,total,fail_count)
  qual_df %>%
    print_large_kable() %>%
    column_spec(1, width = "55%") %>%
    column_spec(2:ncol(glossary), width = "15%")
  qual_df
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

check_id_availability <- function(check_df, check_col, compare_df, compare_col, qual_df, check) {
  
  # check qual_df is exists, otherwise create it
  if (!(exists("qual_df") & is.data.frame(qual_df))) {
    qual_df <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(qual_df) <- c("Check", "Number of entries", "Status", "If check fails : n(%)")
  }
  
  total = nrow(check_df)
  
  unavailable <- check_df[!check_df[[check_col]] %in% compare_df[[compare_col]], ]
  
  check_pass_status = ((unavailable %>% nrow()) ==0)
  fail_count = nrow(unavailable)
  
  qual_df = make_data_quality_df(qual_df,check, check_pass_status,total,fail_count)
  qual_df %>%
    print_large_kable() %>%
    column_spec(1, width = "55%") %>%
    column_spec(2:ncol(glossary), width = "15%")
  qual_df
  
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

check_date_within <- function(check_df, check_date, compare_df, compare_date, check_arg, qual_df, check) {
  
  # check qual_df is exists, otherwise create it
  if (!(exists("qual_df") & is.data.frame(qual_df))) {
    qual_df <- data.frame(matrix(ncol = 4, nrow = 0))
    colnames(qual_df) <- c("Check", "Number of entries", "Status", "If check fails : n(%)")
  }
  
  total = nrow(check_df)
  
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

  check_pass_status = ((df %>% nrow()) ==0)
  fail_count = nrow(df)
  
  qual_df = make_data_quality_df(qual_df,check, check_pass_status,total,fail_count)
  qual_df %>%
    print_large_kable() %>%
    column_spec(1, width = "55%") %>%
    column_spec(2:ncol(glossary), width = "15%")
  qual_df
  
}



#' This function will give the concept_id of a given source field in the mapping sheet
#'
#' @param measure_field the source or measure field that interested on
#' @param mapping_sheet the mapping sheet which have the source field and concept id mappings
#'
#' @return

get_source_concept_id <- function(measure_field, mapping_sheet) {
  concept_id <- mapping_sheet %>%
    filter(Variable == measure_field) %>%
    pull(`Concept ID`) %>%
    unique()
  return(concept_id)
}

#' This function will check whether the given measurement is within the acceptable rate
#'
#' @param measure measure name
#' @param measurement the measurement variable that need to check
#' @param bound_df the data table which have all the measurement bounds
#'
#' @return

check_measure_bounds <- function(measure, measurement, bound_df) {
  lower_bound <- bound_df %>%
    filter(Variable == measure) %>%
    pull(`Lower bound`)
  upper_bound <- bound_df %>%
    filter(Variable == measure) %>%
    pull(`Upper bound`)
  concept_id <- bound_df %>%
    filter(Variable == measure) %>%
    pull(`Concept ID`)

  abnormal_df <- measurement %>% filter((value_as_number < lower_bound | value_as_number > upper_bound) & (measurement_concept_id == concept_id))
  return(abnormal_df)
}


#' This function will add data quality queries and thair status in to a one dataframe
#'
#' @param qual_df  the data frame where to add data quality query and status
#' @param total the denominator of the check
#' @param check the check/query
#' @param status Pass/Fail status
#' @param fail_count fail count, if check pass this is zero
#' @param fail_percentage fail percentage, if check pass this is zero
#'
#' @return

add_new_check <- function(qual_df, total, check, status, fail_count, fail_percentage) {
  new_row <- c(check, total, status, paste(fail_count, paste0(" (", fail_percentage, "%)")))
  qual_df[nrow(qual_df) + 1, ] <- new_row
  return(qual_df)
}


#' This function will do the data quality check and add it to the common dataframe
#'
#' @param df  common dataframe
#' @param total the denominator of the check
#' @param check the check/query
#' @param check_run_status Pass/Fail status
#' @param fail_count fail count, if check pass this is zero
#'
#' @return
make_data_quality_df <- function(df,check,check_run_status,total, fail_count=0){
  check_pass_status = eval(check_pass_status)
  
  if(check_pass_status){
    df <- add_new_check(df, total, check, "Pass", "0", "0")
  } else{
    fail_percentage = format(round(fail_count / total * 100, 4), scientific = FALSE)
    df <- add_new_check(df, total, check, "Fail", fail_count, fail_percentage)
  }
  df
}
