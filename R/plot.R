#' @importFrom dplyr mutate count group_by ungroup if_else
#' @importFrom forcats fct_rev fct_infreq
#' @importFrom stringr str_wrap str_sub
#' @import ggplot2
NULL

#' Package colour palette
#' @export
OMOP_PALETTE <- c("#379481", "#4CA1CB", "#3AB8EE", "#569FB2")

#' Base ggplot2 theme for all ccaa.inspectEHR plots
#' @return A \code{ggplot2::theme} object
#' @export
theme_omop <- function() {
  ggplot2::theme_bw(base_family = "sans", base_size = 11) +
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title          = ggplot2::element_text(colour = "grey40", hjust = 0),
      axis.text           = ggplot2::element_text(colour = "grey50", size = 7),
      axis.title          = ggplot2::element_text(colour = "grey50", size = 9),
      axis.line           = ggplot2::element_blank(),
      panel.border        = ggplot2::element_blank(),
      panel.grid          = ggplot2::element_blank(),
      panel.background    = ggplot2::element_blank(),
      legend.background   = ggplot2::element_blank(),
      legend.key          = ggplot2::element_blank(),
      legend.position     = "bottom",
      strip.background    = ggplot2::element_blank()
    )
}

# ── Person plots ──────────────────────────────────────────────────────────────

#' Plot age distribution as a lollipop chart
#' @param overview Tibble as returned by \code{\link{prepare_overview}}
#' @return A \code{ggplot2} object
#' @export
plot_age <- function(overview) {
  overview %>%
    dplyr::mutate(age = cut(age, c(18, seq(20, 140, by = 5)))) %>%
    dplyr::count(age) %>%
    ggplot2::ggplot(ggplot2::aes(y = age, x = n)) +
    ggplot2::geom_point(colour = OMOP_PALETTE[1]) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = n, yend = age),
                          colour = OMOP_PALETTE[1]) +
    ggplot2::labs(x = "Count", y = "", title = "Age distribution") +
    theme_omop()
}

#' Plot sex distribution as a lollipop chart
#' @param person Tibble: the \code{person} table from \code{\link{prepare_tables}}
#' @return A \code{ggplot2} object
#' @export
plot_sex <- function(person) {
  person %>%
    dplyr::count(gender_concept_id) %>%
    ggplot2::ggplot(ggplot2::aes(y = gender_concept_id, x = n)) +
    ggplot2::geom_point(colour = OMOP_PALETTE[2]) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = n, yend = gender_concept_id),
                          colour = OMOP_PALETTE[2]) +
    ggplot2::labs(x = "Count", y = "", title = "Sex distribution") +
    theme_omop()
}

#' Plot ethnicity distribution as a lollipop chart
#' @param person Tibble: the \code{person} table from \code{\link{prepare_tables}}
#' @return A \code{ggplot2} object
#' @export
plot_ethnicity <- function(person) {
  person %>%
    dplyr::mutate(
      race_concept_id = gsub(
        " - England and Wales ethnic category 2011 census", "",
        race_concept_id
      ),
      race_concept_id = dplyr::if_else(
        nchar(race_concept_id) >= 30,
        paste0(stringr::str_sub(race_concept_id, 1, 29), "\u2026"),
        race_concept_id
      ),
      race_concept_id = forcats::fct_rev(forcats::fct_infreq(race_concept_id))
    ) %>%
    dplyr::count(race_concept_id) %>%
    ggplot2::ggplot(ggplot2::aes(y = race_concept_id, x = n)) +
    ggplot2::geom_point(colour = OMOP_PALETTE[3]) +
    ggplot2::geom_segment(ggplot2::aes(x = 0, xend = n, yend = race_concept_id),
                          colour = OMOP_PALETTE[3]) +
    ggplot2::labs(x = "Count", y = "", title = "Ethnicity distribution") +
    theme_omop()
}

# ── Visit plots ───────────────────────────────────────────────────────────────

#' Plot daily admission counts by visit type over the reporting period
#' @param visit_occurrence Tibble: the \code{visit_occurrence} table
#' @param start_date,end_date Character or Date. Reporting period boundaries
#' @return A \code{ggplot2} object
#' @export
plot_visit_profile <- function(visit_occurrence, start_date, end_date) {
  visit_occurrence %>%
    dplyr::mutate(visit_start_date = as.Date(visit_start_date)) %>%
    dplyr::filter(
      visit_start_date >= as.Date(start_date),
      visit_start_date <= as.Date(end_date)
    ) %>%
    dplyr::count(visit_start_date, visit_concept_id) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = visit_start_date, y = n,
      colour = visit_concept_id, group = visit_concept_id
    )) +
    ggplot2::geom_path() +
    ggplot2::scale_colour_manual(values = OMOP_PALETTE) +
    ggplot2::labs(
      x = "Arrival date", y = "Daily admissions",
      colour = "Visit type", title = "Admission profile by visit type"
    ) +
    theme_omop()
}

#' Plot daily visit detail counts by type over the reporting period
#' @param visit_detail Tibble: the \code{visit_detail} table
#' @param start_date,end_date Character or Date. Reporting period boundaries
#' @return A \code{ggplot2} object
#' @export
plot_visit_detail_profile <- function(visit_detail, start_date, end_date) {
  visit_detail %>%
    dplyr::mutate(
      visit_detail_start_date = as.Date(visit_detail_start_datetime),
      visit_detail_concept_id = as.character(visit_detail_concept_id)
    ) %>%
    dplyr::filter(
      visit_detail_start_date > as.Date(start_date),
      visit_detail_start_date <= as.Date(end_date)
    ) %>%
    dplyr::count(visit_detail_start_date, visit_detail_concept_id) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = visit_detail_start_date, y = n,
      colour = visit_detail_concept_id, group = visit_detail_concept_id
    )) +
    ggplot2::geom_path() +
    ggplot2::scale_colour_manual(values = OMOP_PALETTE) +
    ggplot2::labs(
      x = "Date", y = "Daily patients",
      colour = "Visit type", title = "Daily observation profile by type"
    ) +
    theme_omop()
}

# ── DQ plots ──────────────────────────────────────────────────────────────────

#' Heatmap of all DQ check results
#'
#' Rows are checks, columns are Khan framework categories, faceted by domain.
#' Tiles are green for passing checks and graduate yellow → red by percentage
#' failing.
#'
#' @param dq_df Tibble as accumulated by \code{\link{dq_row}} calls in the DQ
#'   report — columns Domain, Category, Check, N, Fail_n, Fail_pct, Status
#' @return A \code{ggplot2} object
#' @export
plot_dq_heatmap <- function(dq_df) {
  dq_df %>%
    dplyr::mutate(
      label    = dplyr::if_else(
        Status == "Pass", "\u2713", paste0(round(Fail_pct, 1), "%")
      ),
      fill_val = dplyr::if_else(Status == "Pass", 0, Fail_pct),
      Check    = stringr::str_wrap(Check, 42),
      Check    = forcats::fct_rev(factor(Check))
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = Category, y = Check, fill = fill_val)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 2.6, colour = "white") +
    ggplot2::facet_grid(Domain ~ ., scales = "free_y", space = "free_y") +
    ggplot2::scale_fill_gradientn(
      colours = c("#379481", "#f0c040", "#c0392b"),
      limits  = c(0, 100), name = "% failing"
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(
      x = NULL, y = NULL, title = "Data Quality Overview",
      subtitle = "Green = pass \u00b7 yellow/red = % of records failing"
    ) +
    theme_omop() +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(size = 8, angle = 25, hjust = 0),
      axis.text.y     = ggplot2::element_text(size = 7),
      strip.text.y    = ggplot2::element_text(angle = 0, size = 8, hjust = 0),
      legend.position = "right",
      panel.spacing   = ggplot2::unit(0.3, "lines")
    )
}

#' Stacked bar showing pass/fail proportion per DQ domain
#' @param dq_df Tibble as accumulated by \code{\link{dq_row}} calls
#' @return A \code{ggplot2} object
#' @export
plot_dq_summary <- function(dq_df) {
  dq_df %>%
    dplyr::count(Domain, Status) %>%
    dplyr::group_by(Domain) %>%
    dplyr::mutate(pct = n / sum(n) * 100) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(
      x = pct, y = forcats::fct_rev(Domain), fill = Status
    )) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::scale_fill_manual(values = c(Pass = "#379481", Fail = "#c0392b")) +
    ggplot2::labs(x = "% of checks", y = NULL, fill = NULL,
                  title = "Pass / Fail by domain") +
    theme_omop()
}

#' Flag tally rows that are zero (for lollipop chart colouring)
#' @param x A data frame
#' @param column Bare column name containing tally counts
#' @return \code{x} with an added \code{.is_zero} factor column
#' @export
check_zero_tally <- function(x, column) {
  x %>% dplyr::mutate(
    .is_zero = factor(
      dplyr::if_else({{ column }} == 0, "yes", "no"),
      levels = c("yes", "no")
    )
  )
}