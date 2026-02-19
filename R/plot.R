PALETTE <- c("#379481", "#4CA1CB", "#3AB8EE", "#569FB2")

# ── Theme ─────────────────────────────────────────────────────────────────────

theme_omop <- function() {
  theme_bw(base_family = "sans", base_size = 11) +
    theme(
      plot.title.position = "plot",
      plot.title          = element_text(colour = "grey40", hjust = 0),
      axis.text           = element_text(colour = "grey50", size = 7),
      axis.title          = element_text(colour = "grey50", size = 9),
      axis.line           = element_blank(),
      panel.border        = element_blank(),
      panel.grid          = element_blank(),
      panel.background    = element_blank(),
      legend.background   = element_blank(),
      legend.key          = element_blank(),
      legend.position     = "bottom",
      strip.background    = element_blank()
    )
}

# ── Person plots ──────────────────────────────────────────────────────────────

plot_age <- function(overview) {
  overview |>
    mutate(age = cut(age, c(18, seq(20, 140, by = 5)))) |>
    count(age) |>
    ggplot(aes(y = age, x = n)) +
    geom_point(colour = PALETTE[1]) +
    geom_segment(aes(x = 0, xend = n, yend = age), colour = PALETTE[1]) +
    labs(x = "Count", y = "", title = "Age distribution") +
    theme_omop()
}

plot_sex <- function(person) {
  person |>
    count(gender_concept_id) |>
    ggplot(aes(y = gender_concept_id, x = n)) +
    geom_point(colour = PALETTE[2]) +
    geom_segment(aes(x = 0, xend = n, yend = gender_concept_id), colour = PALETTE[2]) +
    labs(x = "Count", y = "", title = "Sex distribution") +
    theme_omop()
}

plot_ethnicity <- function(person) {
  person |>
    mutate(
      race_concept_id = gsub(" - England and Wales ethnic category 2011 census", "", race_concept_id),
      race_concept_id = if_else(nchar(race_concept_id) >= 30,
                                paste0(stringr::str_sub(race_concept_id, 1, 29), "…"),
                                race_concept_id),
      race_concept_id = fct_rev(fct_infreq(race_concept_id))
    ) |>
    count(race_concept_id) |>
    ggplot(aes(y = race_concept_id, x = n)) +
    geom_point(colour = PALETTE[3]) +
    geom_segment(aes(x = 0, xend = n, yend = race_concept_id), colour = PALETTE[3]) +
    labs(x = "Count", y = "", title = "Ethnicity distribution") +
    theme_omop()
}

# ── Visit plots ───────────────────────────────────────────────────────────────

plot_visit_profile <- function(visit_occurrence, start_date, end_date) {
  visit_occurrence |>
    mutate(visit_start_date = as.Date(visit_start_date)) |>
    filter(visit_start_date >= as.Date(start_date),
           visit_start_date <= as.Date(end_date)) |>
    count(visit_start_date, visit_concept_id) |>
    ggplot(aes(x = visit_start_date, y = n,
               colour = visit_concept_id, group = visit_concept_id)) +
    geom_path() +
    scale_colour_manual(values = PALETTE) +
    labs(x = "Arrival date", y = "Daily admissions",
         colour = "Visit type", title = "Admission profile by visit type") +
    theme_omop()
}

plot_visit_detail_profile <- function(visit_detail, start_date, end_date) {
  visit_detail |>
    mutate(visit_detail_start_date = as.Date(visit_detail_start_datetime),
           visit_detail_concept_id = as.character(visit_detail_concept_id)) |>
    filter(visit_detail_start_date > as.Date(start_date),
           visit_detail_start_date <= as.Date(end_date)) |>
    count(visit_detail_start_date, visit_detail_concept_id) |>
    ggplot(aes(x = visit_detail_start_date, y = n,
               colour = visit_detail_concept_id, group = visit_detail_concept_id)) +
    geom_path() +
    scale_colour_manual(values = PALETTE) +
    labs(x = "Date", y = "Daily patients", colour = "Visit type",
         title = "Daily observation profile by type") +
    theme_omop()
}

# ── DQ heatmap ────────────────────────────────────────────────────────────────

#' Plot data quality results as a heatmap
#'
#' @param dq_df result of running DQ checks: columns Domain, Category, Check,
#'   Status ("Pass"/"Fail"), Fail_pct
#' @return a ggplot heatmap
plot_dq_heatmap <- function(dq_df) {
  dq_df |>
    mutate(
      label    = if_else(Status == "Pass", "✓",
                         paste0(round(Fail_pct, 1), "%")),
      fill_val = if_else(Status == "Pass", 0, Fail_pct),
      Check    = stringr::str_wrap(Check, 40),
      Check    = fct_rev(factor(Check))
    ) |>
    ggplot(aes(x = Category, y = Check, fill = fill_val)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = label), size = 2.8, colour = "white") +
    facet_grid(Domain ~ ., scales = "free_y", space = "free_y") +
    scale_fill_gradientn(
      colours = c("#379481", "#f0c040", "#c0392b"),
      limits  = c(0, 100),
      name    = "% failing"
    ) +
    scale_x_discrete(position = "top") +
    labs(x = NULL, y = NULL,
         title = "Data Quality Overview",
         subtitle = "Green = pass · Yellow/Red = % of records failing") +
    theme_omop() +
    theme(
      axis.text.x    = element_text(size = 8, angle = 25, hjust = 0),
      axis.text.y    = element_text(size = 7),
      strip.text.y   = element_text(angle = 0, size = 8, hjust = 0),
      legend.position = "right",
      panel.spacing  = unit(0.3, "lines")
    )
}

#' Plot pass/fail summary bar chart per domain
plot_dq_summary <- function(dq_df) {
  dq_df |>
    count(Domain, Status) |>
    group_by(Domain) |>
    mutate(pct = n / sum(n) * 100) |>
    ggplot(aes(x = pct, y = fct_rev(Domain), fill = Status)) +
    geom_col(width = 0.6) +
    scale_fill_manual(values = c(Pass = "#379481", Fail = "#c0392b")) +
    labs(x = "% of checks", y = NULL, fill = NULL,
         title = "Pass / Fail by domain") +
    theme_omop()
}

#' Measurement completeness heatmap (% of patients with ≥1 measurement per day)
#'
#' @param measurements the measurements table filtered to period
#' @param visit_detail the visit_detail table
#' @param concept_labels named character vector: concept_id → display label
plot_completeness_heatmap <- function(measurements, visit_detail, concept_labels) {
  # All patient-days from visit_detail
  patient_days <- visit_detail |>
    mutate(
      start = as.Date(visit_detail_start_datetime),
      end   = as.Date(visit_detail_end_datetime)
    ) |>
    rowwise() |>
    mutate(day = list(seq(start, end, by = "day"))) |>
    unnest(day) |>
    distinct(person_id, day)
  
  total_days <- nrow(patient_days)
  
  measurements |>
    filter(measurement_concept_id %in% names(concept_labels)) |>
    mutate(day = as.Date(measurement_datetime)) |>
    distinct(person_id, day, measurement_concept_id) |>
    group_by(measurement_concept_id) |>
    summarise(pct_covered = n() / total_days * 100, .groups = "drop") |>
    mutate(label = paste0(round(pct_covered), "%"),
           concept = concept_labels[as.character(measurement_concept_id)]) |>
    ggplot(aes(x = 1, y = fct_reorder(concept, pct_covered), fill = pct_covered)) +
    geom_tile(colour = "white") +
    geom_text(aes(label = label), colour = "white", size = 3) +
    scale_fill_gradientn(
      colours = c("#c0392b", "#f0c040", "#379481"),
      limits  = c(0, 100), name = "% patient-days\nwith ≥1 measurement"
    ) +
    labs(x = NULL, y = NULL,
         title    = "Measurement completeness",
         subtitle = "% of patient-days with at least one measurement recorded") +
    theme_omop() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
}

# ── Misc ──────────────────────────────────────────────────────────────────────

check_zero_tally <- function(x, column) {
  x |> mutate(
    .is_zero = factor(if_else({{ column }} == 0, "yes", "no"),
                      levels = c("yes", "no"))
  )
}

print_large_kable <- function(table, caption = "", max_rows = 100) {
  if (nrow(table) == 0) { cat("\n\n<!-- -->\n\n"); return(invisible(NULL)) }
  nr  <- nrow(table)
  cap <- paste0(caption, " (", min(max_rows, nr), " of ", nr, " rows)")
  table |>
    head(max_rows) |>
    mutate_if(is.character, ~ stringr::str_replace_all(.x, "\\n", "<br>")) |>
    knitr::kable(format.args = list(big.mark = ","), caption = cap,
                 digits = 2, escape = FALSE, format = "html", align = "l") |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                              full_width = TRUE)
}
