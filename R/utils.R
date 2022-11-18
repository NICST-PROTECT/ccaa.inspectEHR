#' Match and replace Athena codes with proper names
#'
#' @param column an integer vector of Athena codes
#' @param lookup an extract of the \code{concept} table with 2 columns:
#'   1. concept_name (containing the full name replacement)
#'   2. concept_id (containing the Athena code)
#'
#' @return
#' @export
match_concepts <- function(column, lookup) {

  if (all(is.na(column))) {
    return(column)
  } else {
    lookup$concept_name[match(column,  lookup$concept_id)]
  }
}

#' Tally OMOP Concept ID Column
#'
#' @param ctn a database connection object
#' @param schema the database schema
#' @param table the OMOP target table
#' @param concept_col the concept_id target column (but be \code{..._concept_id})
#'
#' @importFrom dplyr tbl group_by filter select tally collect mutate across
#' @importFrom dbplyr in_schema
#' @importFrom tidyselect everything
#' @importFrom rlang .data !!
#' @importFrom magrittr `%>%`
#'
#' @return
#' @export
tally_concept <- function(ctn, schema, table, concept_col) {
  x <- tbl(src = ctn, in_schema(schema = schema, table = table)) %>%
    group_by(.data[[concept_col]]) %>%
    tally() %>%
    collect() %>%
    mutate(across(everything(), as.integer))

  dict <- mini_dict(ctn, schema, x[[concept_col]])

  x[[concept_col]] <- match_concepts(x[[concept_col]], dict)

  return(x)
}

#' Create a mini concept dictionary
#'
#' It is convenient to work with a small dictionary of concepts. This function
#' pulls them out and creates a standardised dictionary of concept ids and names
#' for use in other functions.
#'
#' @param ctn a database connection object
#' @param schema the database schema
#' @param concept_ids an integer vector of athena concept ids
#'
#' @importFrom dplyr tbl filter select collect mutate
#' @importFrom dbplyr in_schema
#' @importFrom rlang !! .data
#' @importFrom magrittr `%>%`
#'
#' @return
#' @export
mini_dict <- function(ctn, schema, concept_ids) {

  tbl(src = ctn, in_schema(schema = schema, table = "concept")) %>%
    filter(concept_id %in% !! concept_ids) %>%
    select(.data$concept_id, .data$concept_name) %>%
    collect() %>%
    mutate(concept_id = as.integer(.data$concept_id))
}

#' Summarise Missing Values within a Dataframe
#'
#' @param x a dataframe
#' @param tbl_name the OMOP table name of \code{x}
#'
#' @importFrom dplyr summarise across mutate
#' @importFrom tidyselect everything
#' @importFrom tidyr pivot_longer
#' @importFrom tibble add_column
#' @importFrom magrittr `%>%`
#'
#' @return
#' @export
summarise_missing <- function(x, tbl_name) {
  x %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(),
                 names_to = "column",
                 values_to = "n rows missing") %>%
    add_column(table = tbl_name, .before = TRUE) %>%
    mutate(`% rows missing` = round((`n rows missing`/nrow(x))*100, 0))
}

#' Check which rows contain zero
#'
#' Appends a dataframe with a new column to identify rows that contain zeros.
#' This is then represented as a factor with the levels: "no" and "yes" so that
#' both levels are observed for plotting purposes
#'
#' @param x a dataframe
#' @param column the column name with tally counts (tidyeval)
#'
#' @importFrom rlang .data
#' @importFrom dplyr if_else
#'
#' @return
#' @export
check_zero_tally <- function(x, column) {
  x %>%
    mutate(.is_zero = if_else({{ column }} == 0, "yes", "no")) %>%
    mutate(.is_zero = factor(
      x = .data$.is_zero,
      levels = c("yes", "no"),
      labels = c("yes", "no"))
    )
}

quick_lead <- function(x) {
  if (length(x) == 1) {
    return(NA)
  } else {
    c(x[2:length(x)], NA)
  }
}

outliers <- function(x, probs = 0.99) {
  boundary <- as.numeric(quantile(x, probs = probs))
  if_else(x >= boundary, "outlier", "main")
}

#' Remove elements from ggplot at Glob level
#'
#' @param x the ggplot after being parsed as a gtable
#' @param name the glob element name
#' @param trim logical flag
#'
#' @return
#' @export
#'
#' @importFrom gtable gtable_trim
gtable_filter_remove <- function (x, name, trim = TRUE){
  matches <- !(x$layout$name %in% name)
  x$layout <- x$layout[matches, , drop = FALSE]
  x$grobs <- x$grobs[matches]
  if (trim)
    x <- gtable_trim(x)
  x
}

is.integer64 <- function(x) {
  check_class <- class(x)
  any(class(x) == "integer64")
}

get_db_driver <- function(params) {
  if (params$driver == "PostgreSQL") {
    this_drv <- RPostgres::Postgres
  } else if (params$driver == "ODBC") {
    this_drv <- odbc::odbc
  } else if (params$driver == "SQLite") {
    this_drv <- RSQLite::SQLite
  } else {
    rlang::abort("There is no available DB Driver")
  }
  return(this_drv)
}

setup_ctn <- function(params) {
  this_drv <- get_db_driver(params)

  if (params$local_hospital == "UHB") {
    ctn <- DBI::dbConnect(
      drv = this_drv(),
      driver = "SQL Server",
      server = params$host,
      database = params$dbname)
  } else {
    ctn <- DBI::dbConnect(
      drv = this_drv(),
      host = params$host,
      port = params$port,
      user = params$user,
      password = params$password,
      dbname = params$dbname)
  }

  return(ctn)
}

#' Print large kable. Standardises the formatting of full width kables.
#' Also double checks that we are not printing empty tables.
#' @param table The table to print.
#' @param caption Caption for the table
#' @param max_rows The maximumn number of rows to print. Defaults to 10.
#' @param print_empty_vars Logical indicating if variables with all NAs should be printed
#' @import knitr
#' @import kableExtra
#' @import tidyverse
#' @return A neatly formatted full width kable.
print_large_kable <- function(table, caption =  "", max_rows = 100, print_empty_vars=TRUE){

  if(print_empty_vars == FALSE){
    table <- table %>%
      select_if(~!(all(is.na(.))))
  }
  if(nrow(table) > 0){
    n_rows <- nrow(table)
    options(knitr.kable.NA = "\\-")
    max_rows <- ifelse(max_rows < n_rows, max_rows, n_rows)
    
    table %>%
      head(max_rows) %>%
      # formatting for kable
      mutate_if(is.character, ~ str_replace_all(.x, "\\n", "<br>")) %>%
      mutate_if(is.character, ~ str_replace_all(.x, "\\^", "\\\\^")) %>%
      kable(format.args = list(big.mark = ","),
            caption = paste0(caption, " Printed ", max_rows, " out of ", n_rows),
            digits = 2, escape = FALSE, format = "html", align = "l") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE)
  } else{
    cat('\n\n<!-- -->\n\n')
  }
}

# Setting a default theme
#' @param graph A ggplot object to be formatted
#' @import ggplot2
#' @noRd
custom_theme <- function(graph){
  formatted_graph <- graph + 
    theme_classic() +
    theme(axis.text.x = element_text(color = "azure4", size = 8),
          axis.text.y = element_text(color = "azure4"),
          axis.title.x = element_text(color = "azure4", size = 10),
          axis.title.y = element_text(color = "azure4", size = 10),
          axis.line = element_line(color = "azure4"),
          plot.title = element_text(color = "azure4", hjust = 0.5),
          legend.title = element_blank(),
          legend.text = element_text(face = "italic", color = "azure4"))
  formatted_graph
}
