# ── Database helpers ──────────────────────────────────────────────────────────

get_db_driver <- function(params) {
  switch(params$driver,
         PostgreSQL = RPostgres::Postgres,
         ODBC       = odbc::odbc,
         SQLite     = RSQLite::SQLite,
         rlang::abort(glue::glue("Unknown DB driver: {params$driver}"))
  )
}

setup_ctn <- function(params) {
  drv <- get_db_driver(params)
    DBI::dbConnect(drv(), host = params$host, port = params$port,
                   user = params$user, password = params$password,
                   dbname = params$dbname)
}

# ── OMOP table helpers ────────────────────────────────────────────────────────

get_period_patients <- function(ctn, schema, start_date, end_date) {
  tbl(ctn, in_schema(schema, "observation_period")) |>
    filter(observation_period_start_date >= as.Date(start_date),
           observation_period_end_date   <= as.Date(end_date)) |>
    pull(person_id)
}

mini_dict <- function(ctn, schema, concept_ids) {
  tbl(ctn, in_schema(schema, "concept")) |>
    filter(concept_id %in% !!concept_ids) |>
    select(concept_id, concept_name) |>
    collect() |>
    mutate(concept_id = as.integer(concept_id))
}

match_concepts <- function(column, lookup) {
  if (all(is.na(column))) return(column)
  lookup$concept_name[match(column, lookup$concept_id)]
}

build_lookup <- function(df, ctn, schema) {
  df |>
    select(contains("concept_id")) |>
    pivot_longer(everything(), names_to = "col", values_to = "concept_id") |>
    distinct(concept_id) |>
    pull(concept_id) |>
    (\(ids) mini_dict(ctn, schema, ids))()
}

apply_concept_names <- function(df, ctn, schema) {
  lookup <- build_lookup(df, ctn, schema)
  df |>
    mutate(across(where(is.integer64), as.integer)) |>
    mutate(across(
      c(contains("concept_id") &
          (!contains("source") | contains("admitting_source_concept_id"))),
      \(x) match_concepts(x, lookup)
    )) |>
    mutate(across(c(contains("date"), -contains("datetime")), as.Date))
}

prepare_tables <- function(ctn, schema) {
  tbls   <- c("person", "visit_occurrence", "death")
  st     <- map(tbls, ~ collect(tbl(ctn, in_schema(schema, .x)))) |> setNames(tbls)
  lookup <- st |>
    map(~ select(., contains("concept_id")) |>
          pivot_longer(everything(), names_to = "col", values_to = "concept_id")) |>
    bind_rows() |>
    distinct(concept_id) |>
    pull(concept_id) |>
    (\(ids) mini_dict(ctn, schema, ids))()
  
  st |>
    map(~ mutate(., across(where(is.integer64), as.integer))) |>
    map(~ mutate(., across(
      c(contains("concept_id") &
          (!contains("source") | contains("admitting_source_concept_id"))),
      \(x) match_concepts(x, lookup)
    ))) |>
    map(~ mutate(., across(c(contains("date"), -contains("datetime")), as.Date)))
}

prepare_overview <- function(st) {
  st[["person"]] |>
    select(person_id, gender_concept_id, year_of_birth) |>
    left_join(
      st[["death"]] |> select(person_id, death_date, death_datetime),
      by = "person_id"
    ) |>
    left_join(
      st[["visit_occurrence"]] |>
        select(person_id, visit_occurrence_id, visit_start_datetime,
               visit_end_datetime, visit_concept_id,
               any_of(c("admitted_from_concept_id", "admitting_source_concept_id")),
               any_of(c("discharged_to_concept_id", "discharge_to_concept_id"))),
      by = "person_id"
    ) |>
    mutate(age = lubridate::interval(
      start = as.Date(paste0(year_of_birth, "-06-15")),
      end   = visit_start_datetime
    ) / lubridate::years(1))
}

prepare_tally <- function(ctn, schema, filter_person_ids, filter_care_sites,
                          tbl_names = c("person", "care_site", "death",
                                        "visit_occurrence", "procedure_occurrence",
                                        "drug_exposure", "condition_occurrence",
                                        "measurement", "observation")) {
  tibble(table = tbl_names) |>
    mutate(n = map_int(table, ~ {
      t <- tbl(ctn, in_schema(schema, .x))
      if ("person_id"    %in% colnames(t)) t <- filter(t, person_id    %in% filter_person_ids)
      else if ("care_site_id" %in% colnames(t)) t <- filter(t, care_site_id %in% filter_care_sites)
      tally(t) |> collect() |> pull() |> as.integer()
    }))
}

# ── Data quality helpers ──────────────────────────────────────────────────────

find_duplicates <- function(df, cols) {
  df |> filter(duplicated(across(all_of(cols))))
}

check_id_availability <- function(check_df, check_col, compare_df, compare_col) {
  check_df |> filter(!.data[[check_col]] %in% compare_df[[compare_col]])
}

check_date_within <- function(check_df, check_date, compare_df = NULL,
                              compare_date, check_arg) {
  cmp <- if (is.null(compare_df)) {
    tibble(person_id = check_df$person_id, compare = as.Date(compare_date))
  } else {
    compare_df |> transmute(person_id, compare = .data[[compare_date]])
  }
  check_df |>
    transmute(person_id, check = .data[[check_date]]) |>
    left_join(cmp, by = "person_id") |>
    filter(eval(parse(text = paste("check", check_arg, "compare"))))
}

# Returns a single-row tibble suitable for bind_rows() onto the DQ frame
dq_row <- function(domain, category, check, total, fail_n) {
  tibble(
    Domain   = domain,
    Category = category,
    Check    = check,
    N        = as.integer(total),
    Fail_n   = as.integer(fail_n),
    Fail_pct = round(fail_n / total * 100, 2),
    Status   = if_else(fail_n == 0L, "Pass", "Fail")
  )
}

empty_dq_df <- function() {
  tibble(Domain = character(), Category = character(), Check = character(),
         N = integer(), Fail_n = integer(), Fail_pct = numeric(),
         Status = character())
}

# ── Measurement helpers ───────────────────────────────────────────────────────

# Returns a tidy DQ tibble for all measurement plausibility checks in one pass,
# joining the bounds table against the measurements table — no loops.
dq_measurement_plausibility <- function(measurements, measure_bounds, tolerance) {
  measurements |>
    filter(!is.na(value_as_number)) |>
    inner_join(
      measure_bounds |>
        select(concept_id = `Concept ID`, Name,
               lower = `Lower bound`, upper = `Upper bound`,
               unit  = `Measure unit`),
      by = c("measurement_concept_id" = "concept_id")
    ) |>
    group_by(Name, lower, upper, unit) |>
    summarise(
      N        = n(),
      Fail_n   = sum(value_as_number < lower | value_as_number > upper),
      .groups  = "drop"
    ) |>
    mutate(
      Domain   = "Atemporal plausibility",
      Category = "Measurement",
      Check    = glue::glue("{Name} within {lower}–{upper} {unit}"),
      Fail_pct = round(Fail_n / N * 100, 2),
      Status   = if_else(Fail_pct <= tolerance, "Pass", "Fail")
    ) |>
    select(Domain, Category, Check, N, Fail_n, Fail_pct, Status)
}