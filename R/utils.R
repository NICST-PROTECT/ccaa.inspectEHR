#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate transmute left_join inner_join
#'   anti_join bind_rows distinct pull collect tally count group_by summarise
#'   ungroup across where all_of any_of if_else tibble
#' @importFrom tidyr pivot_longer unnest
#' @importFrom purrr map map_int
#' @importFrom dbplyr in_schema
#' @importFrom rlang .data !!
#' @importFrom lubridate interval years
#' @importFrom stringr str_replace_all
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling
NULL

# ── Internal constants ────────────────────────────────────────────────────────

.CONCEPT_COLS <- c(
  "contains('concept_id') & (!contains('source') | contains('admitting_source_concept_id'))"
)

.is_integer64 <- function(x) inherits(x, "integer64")

# ── Database ──────────────────────────────────────────────────────────────────

#' Get a DBI driver from a params list
#' @param params named list with element \code{driver}: one of
#'   \code{"PostgreSQL"}, \code{"ODBC"}, \code{"SQLite"}
#' @return A DBI driver function
#' @export
get_db_driver <- function(params) {
  switch(params$driver,
         PostgreSQL = RPostgres::Postgres,
         ODBC       = odbc::odbc,
         SQLite     = RSQLite::SQLite,
         rlang::abort(paste0("Unknown DB driver: ", params$driver))
  )
}

#' Open a database connection from a params list
#' @param params named list of connection parameters (driver, host, port,
#'   dbname, schema, user, password, local_hospital)
#' @return A \code{DBIConnection} object
#' @export
setup_ctn <- function(params) {
  drv <- get_db_driver(params)
  if (params$local_hospital == "UHB") {
    DBI::dbConnect(drv(), driver = "SQL Server",
                   server = params$host, database = params$dbname)
  } else {
    DBI::dbConnect(drv(), host = params$host, port = params$port,
                   user = params$user, password = params$password,
                   dbname = params$dbname)
  }
}

# ── OMOP table helpers ────────────────────────────────────────────────────────

#' Get person IDs active within a reporting period
#' @param ctn A \code{DBIConnection}
#' @param schema Character. Target OMOP schema
#' @param start_date,end_date Character or Date. Reporting period boundaries
#' @return Integer vector of person_ids
#' @export
get_period_patients <- function(ctn, schema, start_date, end_date) {
  dplyr::tbl(ctn, dbplyr::in_schema(schema, "observation_period")) %>%
    dplyr::filter(
      observation_period_start_date >= as.Date(start_date),
      observation_period_end_date   <= as.Date(end_date)
    ) %>%
    dplyr::pull(person_id)
}

#' Pull a small concept-name lookup from the OMOP concept table
#' @param ctn A \code{DBIConnection}
#' @param schema Character. Target OMOP schema
#' @param concept_ids Integer vector of concept IDs to retrieve
#' @return Tibble with columns \code{concept_id} (integer) and
#'   \code{concept_name} (character)
#' @export
mini_dict <- function(ctn, schema, concept_ids) {
  dplyr::tbl(ctn, dbplyr::in_schema(schema, "concept")) %>%
    dplyr::filter(concept_id %in% !!concept_ids) %>%
    dplyr::select(concept_id, concept_name) %>%
    dplyr::collect() %>%
    dplyr::mutate(concept_id = as.integer(concept_id))
}

#' Replace integer concept IDs with human-readable names
#' @param column Integer vector of concept IDs
#' @param lookup Two-column tibble from \code{\link{mini_dict}}
#' @return Character vector of concept names (NAs preserved)
#' @export
match_concepts <- function(column, lookup) {
  if (all(is.na(column))) return(column)
  lookup$concept_name[match(column, lookup$concept_id)]
}

#' Translate all concept_id columns in a data frame to concept names
#' @param df A data frame containing \code{*_concept_id} columns
#' @param ctn A \code{DBIConnection}
#' @param schema Character. Target OMOP schema
#' @return \code{df} with concept_id columns replaced by concept names,
#'   and date columns coerced to \code{Date}
#' @export
apply_concept_names <- function(df, ctn, schema) {
  ids <- df %>%
    dplyr::select(dplyr::contains("concept_id")) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "col", values_to = "concept_id") %>%
    dplyr::distinct(concept_id) %>%
    dplyr::pull(concept_id)
  
  lookup <- mini_dict(ctn, schema, ids)
  
  df %>%
    dplyr::mutate(dplyr::across(where(.is_integer64), as.integer)) %>%
    dplyr::mutate(dplyr::across(
      c(dplyr::contains("concept_id") &
          (!dplyr::contains("source") |
             dplyr::contains("admitting_source_concept_id"))),
      ~ match_concepts(.x, lookup)
    )) %>%
    dplyr::mutate(dplyr::across(
      c(dplyr::contains("date"), -dplyr::contains("datetime")), as.Date
    ))
}

#' Collect and prepare the core OMOP tables (person, visit_occurrence, death)
#' @param ctn A \code{DBIConnection}
#' @param schema Character. Target OMOP schema
#' @return Named list of tibbles: \code{person}, \code{visit_occurrence},
#'   \code{death} — with concept IDs replaced by names and dates coerced
#' @export
prepare_tables <- function(ctn, schema) {
  tbls <- c("person", "visit_occurrence", "death")
  st   <- purrr::map(tbls, ~ dplyr::collect(dplyr::tbl(ctn, dbplyr::in_schema(schema, .x)))) %>%
    setNames(tbls)
  
  all_ids <- purrr::map(
    st,
    ~ dplyr::select(.x, dplyr::contains("concept_id")) %>%
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = "col", values_to = "concept_id")
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::distinct(concept_id) %>%
    dplyr::pull(concept_id)
  
  lookup <- mini_dict(ctn, schema, all_ids)
  
  purrr::map(st, function(tbl) {
    tbl %>%
      dplyr::mutate(dplyr::across(where(.is_integer64), as.integer)) %>%
      dplyr::mutate(dplyr::across(
        c(dplyr::contains("concept_id") &
            (!dplyr::contains("source") |
               dplyr::contains("admitting_source_concept_id"))),
        ~ match_concepts(.x, lookup)
      )) %>%
      dplyr::mutate(dplyr::across(
        c(dplyr::contains("date"), -dplyr::contains("datetime")), as.Date
      ))
  })
}

#' Build a joined overview table (person + death + visit_occurrence)
#' @param st Named list of tibbles as returned by \code{\link{prepare_tables}}
#' @return Tibble with one row per person × visit, including a computed
#'   \code{age} column (years at visit start)
#' @export
prepare_overview <- function(st) {
  st[["person"]] %>%
    dplyr::select(person_id, gender_concept_id, year_of_birth) %>%
    dplyr::left_join(
      st[["death"]] %>% dplyr::select(person_id, death_date, death_datetime),
      by = "person_id"
    ) %>%
    dplyr::left_join(
      st[["visit_occurrence"]] %>%
        dplyr::select(
          person_id, visit_occurrence_id,
          visit_start_datetime, visit_end_datetime, visit_concept_id,
          dplyr::any_of(c("admitted_from_concept_id", "admitting_source_concept_id")),
          dplyr::any_of(c("discharged_to_concept_id", "discharge_to_concept_id"))
        ),
      by = "person_id"
    ) %>%
    dplyr::mutate(age = lubridate::interval(
      start = as.Date(paste0(year_of_birth, "-06-15")),
      end   = visit_start_datetime
    ) / lubridate::years(1))
}

#' Count rows in key OMOP tables, filtered to the reporting period
#' @param ctn A \code{DBIConnection}
#' @param schema Character. Target OMOP schema
#' @param filter_person_ids Integer vector of person IDs to include
#' @param filter_care_sites Integer vector of care_site IDs to include
#' @param tbl_names Character vector of table names to tally
#' @return Tibble with columns \code{table} and \code{n}
#' @export
prepare_tally <- function(ctn, schema, filter_person_ids, filter_care_sites,
                          tbl_names = c(
                            "person", "care_site", "death",
                            "visit_occurrence", "procedure_occurrence",
                            "drug_exposure", "condition_occurrence",
                            "measurement", "observation"
                          )) {
  dplyr::tibble(table = tbl_names) %>%
    dplyr::mutate(n = purrr::map_int(table, function(tbl_name) {
      t <- dplyr::tbl(ctn, dbplyr::in_schema(schema, tbl_name))
      if      ("person_id"    %in% colnames(t)) t <- dplyr::filter(t, person_id    %in% filter_person_ids)
      else if ("care_site_id" %in% colnames(t)) t <- dplyr::filter(t, care_site_id %in% filter_care_sites)
      dplyr::tally(t) %>% dplyr::collect() %>% dplyr::pull() %>% as.integer()
    }))
}

# ── DQ helpers ────────────────────────────────────────────────────────────────

#' Find duplicate rows based on a set of columns
#' @param df A data frame
#' @param cols Character vector of column names to check for duplication
#' @return Rows of \code{df} that are duplicates
#' @export
find_duplicates <- function(df, cols) {
  df %>% dplyr::filter(duplicated(dplyr::across(dplyr::all_of(cols))))
}

#' Find rows in \code{check_df} whose key is absent from \code{compare_df}
#' @param check_df,compare_df Data frames
#' @param check_col,compare_col Column names (character) to compare
#' @return Rows of \code{check_df} with no matching value in \code{compare_df}
#' @export
check_id_availability <- function(check_df, check_col, compare_df, compare_col) {
  check_df %>% dplyr::filter(!.data[[check_col]] %in% compare_df[[compare_col]])
}

#' Find rows where a column contains NA
#'
#' Value conformance / completeness check. Returns rows of \code{df} where
#' \code{column} is \code{NA}.
#'
#' @param df A data frame
#' @param column Character. Column name to check for missingness
#' @return Rows of \code{df} where \code{column} is \code{NA}
#' @export
check_empty <- function(df, column) {
  df %>% dplyr::filter(is.na(.data[[column]]))
}

#' Find rows where a numeric column falls outside an expected range
#'
#' Value conformance check. Returns rows of \code{df} where \code{column} is
#' less than \code{min_val} or greater than \code{max_val}.
#'
#' @param df A data frame
#' @param column Character. Column name to range-check (coerced to numeric)
#' @param min_val Numeric. Lower bound (inclusive)
#' @param max_val Numeric. Upper bound (inclusive)
#' @return Rows of \code{df} where \code{column} is out of range
#' @export
check_number_within <- function(df, column, min_val, max_val) {
  df %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(column), as.numeric)) %>%
    dplyr::filter(.data[[column]] < min_val | .data[[column]] > max_val)
}

#' Find rows where a date column violates a relational constraint
#'
#' Joins \code{check_df} to \code{compare_df} (or a scalar date) on
#' \code{person_id} and returns rows where
#' \code{check_date <check_arg> compare_date} is TRUE.
#'
#' @param check_df Data frame containing \code{person_id} and \code{check_date}
#' @param check_date Character. Column name in \code{check_df} to test
#' @param compare_df Optional data frame containing \code{person_id} and
#'   \code{compare_date}. If \code{NULL}, \code{compare_date} must be a scalar
#'   Date or coercible character
#' @param compare_date Column name in \code{compare_df}, or scalar date when
#'   \code{compare_df} is \code{NULL}
#' @param check_arg Operator string: one of \code{">"}, \code{">="}, \code{"<"},
#'   \code{"<="}, \code{"!="}, \code{"=="}
#' @return Rows of \code{check_df} (with columns \code{person_id} and
#'   \code{check}) where the constraint is violated
#' @export
check_date_within <- function(check_df, check_date, compare_df = NULL,
                              compare_date, check_arg) {
  cmp <- if (is.null(compare_df)) {
    dplyr::tibble(person_id = check_df$person_id, compare = as.Date(compare_date))
  } else {
    compare_df %>% dplyr::transmute(person_id, compare = .data[[compare_date]])
  }
  check_df %>%
    dplyr::transmute(person_id, check = .data[[check_date]]) %>%
    dplyr::left_join(cmp, by = "person_id") %>%
    dplyr::filter(eval(parse(text = paste("check", check_arg, "compare"))))
}

#' Construct a single DQ result row
#'
#' Returns a one-row tibble that can be accumulated with \code{bind_rows()}
#' into the report's running DQ results frame.
#'
#' @param domain Character. Khan framework domain (e.g. \code{"Completeness"})
#' @param category Character. OMOP table or grouping (e.g. \code{"Person"})
#' @param check Character. Human-readable description of the check
#' @param total Integer. Denominator (total records checked)
#' @param fail_n Integer. Number of records failing the check
#' @return One-row tibble with columns Domain, Category, Check, N, Fail_n,
#'   Fail_pct, Status
#' @export
dq_row <- function(domain, category, check, total, fail_n) {
  # Accept a data frame piped in via magrittr (.) or a pre-computed integer
  n <- if (is.data.frame(fail_n)) nrow(fail_n) else as.integer(fail_n)
  dplyr::tibble(
    Domain   = domain,
    Category = category,
    Check    = check,
    N        = as.integer(total),
    Fail_n   = n,
    Fail_pct = round(as.numeric(n) / as.numeric(total) * 100, 2),
    Status   = dplyr::if_else(n == 0L, "Pass", "Fail")
  )
}

#' Empty DQ results frame
#' @return Zero-row tibble with the DQ results schema
#' @export
empty_dq_df <- function() {
  dplyr::tibble(
    Domain = character(), Category = character(), Check = character(),
    N = integer(), Fail_n = integer(), Fail_pct = numeric(),
    Status = character()
  )
}

#' Run all measurement plausibility checks in a single vectorised join
#'
#' No loops — joins the measurements table against the bounds lookup once,
#' then aggregates.
#'
#' @param measurements Tibble of OMOP \code{measurement} rows
#' @param measure_bounds Tibble with columns \code{Concept ID}, \code{Name},
#'   \code{Lower bound}, \code{Upper bound}, \code{Measure unit}
#' @param tolerance Numeric. Percentage tolerance below which a check still
#'   passes (e.g. \code{5} = up to 5\% out-of-range is acceptable)
#' @return Tibble of DQ rows (same schema as \code{\link{empty_dq_df}})
#' @export
dq_measurement_plausibility <- function(measurements, measure_bounds, tolerance) {
  measurements %>%
    dplyr::filter(!is.na(value_as_number)) %>%
    dplyr::inner_join(
      measure_bounds %>%
        dplyr::transmute(
          measurement_concept_id = `Concept ID`,
          Name,
          unit  = `Measure unit`,
          lower = `Lower bound`,
          upper = `Upper bound`
        ),
      by = "measurement_concept_id"
    ) %>%
    dplyr::group_by(Name, lower, upper, unit) %>%
    dplyr::summarise(
      N      = dplyr::n(),
      Fail_n = sum(value_as_number < lower | value_as_number > upper),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      Domain   = "Atemporal plausibility",
      Category = "Measurement",
      Check    = paste0(Name, " within ", lower, "\u2013", upper, " ", unit),
      Fail_pct = round(Fail_n / N * 100, 2),
      Status   = dplyr::if_else(Fail_pct <= tolerance, "Pass", "Fail")
    ) %>%
    dplyr::select(Domain, Category, Check, N, Fail_n, Fail_pct, Status)
}

#' Print a formatted HTML kable, truncated to \code{max_rows}
#' @param table A data frame
#' @param caption Character. Optional caption prefix
#' @param max_rows Integer. Maximum rows to print (default 100)
#' @return A \code{kableExtra} HTML table, or invisible NULL if \code{table}
#'   has zero rows
#' @export
print_large_kable <- function(table, caption = "", max_rows = 100) {
  if (nrow(table) == 0) {
    cat("\n\n<!-- -->\n\n")
    return(invisible(NULL))
  }
  nr  <- nrow(table)
  cap <- paste0(caption, " (", min(max_rows, nr), " of ", nr, " rows)")
  table %>%
    head(max_rows) %>%
    dplyr::mutate(dplyr::across(
      where(is.character), ~ stringr::str_replace_all(.x, "\\n", "<br>")
    )) %>%
    knitr::kable(
      format.args = list(big.mark = ","), caption = cap,
      digits = 2, escape = FALSE, format = "html", align = "l"
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE
    )
}