#' Plot Age
#'
#' @param x
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate group_by tally
#' @importFrom ggplot2 ggplot aes geom_point geom_segment labs theme ggtitle
#'   element_blank
#'
#' @return
#'
#' @examples
plot_age <- function(x) {
  x %>%
    mutate(age = cut(age, c(18, seq(20, 140, by = 5)))) %>%
    group_by(age) %>%
    tally() %>%
    ggplot(aes(y = age)) +
    geom_point(aes(x = n)) +
    geom_segment(aes(
      x = 0,
      xend = n,
      yend = age
    )) +
    theme_d() +
    labs(y = "Age", x = "Count") +
    theme(axis.title.y = element_blank()) +
    ggtitle("Age distribution")
}

#' Plot Sex
#'
#'
#' @param x
#'
#' @return
#'
#' @examples
plot_sex <- function(x) {
  x %>%
    group_by(gender_concept_id) %>%
    tally() %>%
    ggplot(aes(y = gender_concept_id)) +
    geom_point(aes(x = n)) +
    geom_segment(aes(
      x = 0,
      xend = n,
      yend = gender_concept_id
    )) +
    labs(x = "Count", y = "Category") +
    theme_d() +
    theme(axis.title.y = element_blank()) +
    ggtitle("Sex distribution")
}


#' Plot Ethnicity
#'
#'
#' @param x
#'
#' @importFrom dplyr mutate if_else group_by tally
#' @importFrom forcats fct_infreq fct_rev
#' @importFrom ggplot2 ggplot aes geom_point geom_segment labs theme ggtitle
#'
#' @return
#'
#' @examples
plot_ethnicity <- function(x) {
  x %>%
    mutate(
      race_concept_id = gsub(
        " - England and Wales ethnic category 2011 census",
        "",
        race_concept_id
      )
    ) %>%
    mutate(
      race_concept_id = if_else(
        nchar(race_concept_id) >= 30,
        paste0(stringr::str_sub(race_concept_id, 1, 29), "..."),
        race_concept_id
      )
    ) %>%
    mutate(
      race_concept_id = forcats::fct_rev(
        forcats::fct_infreq(race_concept_id)
      )
    ) %>%
    group_by(race_concept_id) %>%
    tally() %>%
    ggplot(aes(y = race_concept_id)) +
    geom_point(aes(x = n)) +
    geom_segment(aes(
      x = 0,
      xend = n,
      yend = race_concept_id
    )) +
    labs(x = "Count", y = "Category") +
    theme_d() +
    theme(axis.title.y = element_blank()) +
    ggtitle("Ethnicity distribution")
}

#' Plot Visit Profile
#'
#' @param x
#' @param start_date starting date for x axis
#' @param end_date ending dare for x axis
#'
#' @importFrom dplyr mutate_at vars group_by tally
#' @importFrom ggplot2 ggplot aes geom_path theme labs
#'
#' @return
#'
#' @examples
plot_visit_profile <- function(x, start_date, end_date, custom_colors) {
  x %>%
    mutate_at(vars(visit_start_date), ~ as.Date(.)) %>%
    # Shouldn't really hardcode these dates. Will edit as we set tolerance for data quality report.
    filter((visit_start_date >= as.Date(start_date, format = "%Y-%m-%d")) & (visit_start_date <= as.Date(end_date, format = "%Y-%m-%d"))) %>%
    group_by(visit_start_date, visit_concept_id) %>%
    tally() %>%
    ggplot(aes(
      x = visit_start_date, y = n,
      group = visit_concept_id, colour = visit_concept_id
    )) +
    geom_path() +
    theme_d() +
    labs(y = "Daily number of patient attendances", x = "Arrival date", colour = "Visit type") +
    theme(legend.position = "bottom") +
    ggtitle("Admission profile by type of admission") +
    scale_color_manual(values = custom_colors)
}

#' Plot Visit detail Profile
#'
#' @param x
#' @param start_date starting date for x axis
#' @param end_date ending dare for x axis
#'
#' @importFrom dplyr mutate_at vars group_by tally
#' @importFrom ggplot2 ggplot aes geom_path theme labs
#'
#' @return
#'
#' @examples
plot_visit_detail_profile <- function(x, start_date, end_date, custom_colors) {
  x %>%
    mutate_at(vars(visit_detail_start_datetime), ~ as.Date(.)) %>%
    mutate_at(vars(visit_detail_concept_id), ~ as.character(.)) %>%
    # Shouldn't really hardcode these dates. Will edit as we set tolerance for data quality report.
    filter((visit_detail_start_date > as.Date(start_date, format = "%Y-%m-%d")) & (visit_detail_start_date <= as.Date(end_date, format = "%Y-%m-%d"))) %>%
    group_by(visit_detail_start_date, visit_detail_concept_id) %>%
    tally() %>%
    ggplot(aes(
      x = visit_detail_start_date, y = n,
      group = visit_detail_concept_id, colour = visit_detail_concept_id
    )) +
    geom_path() +
    theme_d() +
    labs(y = "Daily number of patients in visit details", x = "Date", colour = "Visit type") +
    theme(legend.position = "bottom") +
    ggtitle("Daily observation profile by type of observation") +
    scale_color_manual(values = custom_colors)
}

#' Plot Theme
#'
#' @param ... arguments to pass to \code{theme}
#'
#' @importFrom ggplot2 %+replace% theme theme_bw element_blank
#'
#' @return
#'
#' @examples
theme_d <- function(...) {
  pct <- theme_bw(base_family = "sans", base_size = 11) %+replace%
    theme(
      plot.title.position = "plot",
      legend.background = element_blank(),
      legend.key = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      strip.background = element_blank(),
      plot.background = element_blank(),
      axis.line = element_blank(),
      panel.grid = element_blank()
    )
}
