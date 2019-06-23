#' Create a Weekly Planner
#'
#' Creates a weekly calendar with ggplot2
#'
#' @param start_day The starting day, either as a YYYY-mm-dd text string or
#'   using [lubridate::ymd()].
#' @param end_day The ending day, either as a YYYY-mm-dd text string or
#'   using [lubridate::ymd()].
#' @param highlight_days Special days to be highlighted, formatted as a tibble
#'   with columns `day`, `label`, `color`, and `fill`.
#' @param week_start_label What labels should be used for the week start, i.e.
#'   to the left of the first day of the week? One of `"month day"`,
#'   `"isoweek"`, or `"none"`.
#' @param show_day_numbers Should day numbers be included in each box of the
#'   calendar?
#' @param show_month_start_day Should the first day of the month be highlighted?
#' @param show_month_boundaries Should boundaries between months be highlighted
#'   with a dotted line?
#' @param highlight_text_size Size of the text in the highlighted days.
#' @param month_text_size The size of the text labelling the first day of the month.
#' @param month_color The color of the boxes highlighting the first day of the month.
#' @param holidays A tibble containing holiday dates in the same format as
#'   `higlight_days`. Defaults to a list of US Federal Holidays. Set to `NULL`
#'   to disable.
#' @param font_label_text Font for label text, default is
#'   [PT Sans Narrow](https://fonts.google.com/specimen/PT+Sans+Narrow).
#' @export
ggweek_planner <- function(
  start_day = lubridate::today(),
  end_day = start_day + lubridate::weeks(8) - lubridate::days(1),
  highlight_days = NULL,
  week_start_label = c("month day", "isoweek", "none"),
  show_day_numbers = TRUE,
  show_month_start_day = TRUE,
  show_month_boundaries = TRUE,
  highlight_text_size = 2,
  month_text_size = 4,
  month_color = "#f78154",
  holidays = us_federal_holidays,
  font_label_text = "PT Sans Narrow"
) {
  old_opts <- options("lubridate.week.start" = 1)
  on.exit(options(old_opts))

  if (!inherits(start_day, "Date")) {
    start_day <- lubridate::ymd(start_day)
  }
  if (!inherits(end_day, "Date")) {
    end_day <- lubridate::ymd(end_day)
  }

  start_day <- start_day[1]
  end_day <- end_day[1]

  force(end_day)
  seq_days  <- seq(start_day, end_day, by = "day")

  dates <-
    dplyr::tibble(
      day       = seq_days,
      wday_name = lubridate::wday(day, label = TRUE, abbr = TRUE),
      weekend   = lubridate::wday(day) > 5,
      isoweek   = lubridate::isoweek(day),
      month     = lubridate::month(day, label = TRUE, abbr = FALSE),
      isoyear   = lubridate::isoyear(day),
      week_year = sprintf("%s - %s", isoyear, isoweek)
    ) %>%
    dplyr::mutate(
      week_year = forcats::fct_inorder(week_year),
      week_year = forcats::fct_rev(week_year)
    )

  day_one <- dates %>%
    dplyr::filter(lubridate::day(day) == 1)

  gcal <-
    dates %>%
    dplyr::mutate(
      # Softly fill in the weekend days
      weekend = dplyr::case_when(weekend ~ "#f8f8f8", TRUE ~ "#FFFFFF")
    ) %>%
    dplyr::arrange(day) %>%
    ggplot2::ggplot() +
    ggplot2::aes(wday_name, week_year) +
    ggplot2::geom_tile(ggplot2::aes(fill = weekend), color = "#aaaaaa") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::guides(fill = FALSE) +
    ggplot2::theme_minimal(base_family = "PT Sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text = ggplot2::element_text(font_label_text),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      # axis.text.x.top = ggplot2::element_text(face = "bold"),
      axis.title.x.top = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(color = "grey50"),
      strip.placement = "outside"
    )

  week_start_label <- match.arg(week_start_label)

  if (week_start_label == "month day") {
    gcal <- gcal +
      ggplot2::scale_y_discrete(
        breaks = levels(dates$week_year),
        labels = week_start_labels(dates)
      )
  } else if (week_start_label == "isoweek") {
    gcal <- gcal +
      ggplot2::scale_y_discrete(
        breaks = levels(dates$week_year),
        labels = sub("^\\d{4} - ", "", levels(dates$week_year))
      )
  }

  if (show_day_numbers) {
    gcal <- gcal +
      ggplot2::geom_text(
        ggplot2::aes(label = lubridate::day(day)),
        family = font_label_text,
        color = "grey80",
        size = 2,
        hjust = 1,
        nudge_x = 0.45,
        vjust = 1,
        nudge_y = 0.45
      )
  }

  if (show_month_start_day) {
    gcal <- gcal +
      ggplot2::geom_tile(
        data = day_one,
        fill = month_color,
        alpha = 0.25
      ) +
      ggplot2::geom_text(
        data = day_one,
        ggplot2::aes(label = month),
        family = font_label_text,
        color = month_color,
        size = month_text_size,
        hjust = 0,
        nudge_x = -0.45,
        vjust = 1,
        nudge_y = 0.45
      )
  }

  if (!is.null(holidays)) {
    holidays <- ensure_highlight_day_cols(holidays, show_month_start_day)

    gcal <- gcal +
      ggplot2::geom_tile(
        data = dates %>% dplyr::inner_join(holidays, by = "day"),
        ggplot2::aes(fill = fill),
        color = NA,
        alpha = 0.25
      ) +
      ggplot2::geom_text(
        data = dates %>% dplyr::inner_join(holidays, by = "day"),
        ggplot2::aes(label = label, color = color),
        family = font_label_text,
        size = highlight_text_size,
        hjust = 0,
        nudge_x = -0.45,
        vjust = 0,
        nudge_y = -0.40
      )
  }

  if (!is.null(highlight_days)) {
    highlight_days <- ensure_highlight_day_cols(highlight_days, show_month_start_day)

    gcal <- gcal +
      ggplot2::geom_tile(
        data = dates %>% dplyr::inner_join(highlight_days, by = "day"),
        ggplot2::aes(fill = fill),
        color = NA,
        alpha = 0.25
      ) +
      ggplot2::geom_text(
        data = dates %>% dplyr::inner_join(highlight_days, by = "day"),
        ggplot2::aes(label = label, color = color),
        family = "PT Sans Narrow",
        size = highlight_text_size,
        hjust = 0,
        nudge_x = -0.45,
        vjust = 0,
        nudge_y = -0.40
      )
  }

  if (show_month_boundaries) {
    month_boundaries <- day_one %>%
      generate_month_boundaries()

    for (boundary in c("left", "up", "right")) {
      gcal <- gcal +
        ggplot2::geom_segment(
          data = month_boundaries %>% tidyr::unnest(!!rlang::sym(boundary)),
          ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
          color = "#999999",
          linetype = 2
        )
    }
  }

  gcal
}

week_start_labels <- function(dates) {
  dates %>%
    dplyr::arrange(day) %>%
    dplyr::group_by(week_year) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(day) %>%
    dplyr::mutate(
      day = lubridate::floor_date(day, "week", week_start = 1),
      month = lubridate::month(day, label = TRUE),
      label = dplyr::case_when(
        month == dplyr::lag(month) ~ paste(lubridate::day(day)),
        TRUE ~ sprintf("%s %4i", month, lubridate::day(day))
      )
    ) %>%
    dplyr::select(label, week_year) %>%
    purrr::reduce(purrr::set_names)
}

generate_month_boundaries <- function(dates) {
  dates %>%
    dplyr::filter(lubridate::day(day) == 1) %>%
    dplyr::select(day, month, wday_name, week_year) %>%
    dplyr::mutate_at(dplyr::vars(wday_name, week_year), as.integer) %>%
    dplyr::mutate(
      left = purrr::map2(wday_name, week_year, ~ {
        # n/a if month changes on first day
        if (.x == 1) return(dplyr::tibble(.missing = NA))
        dplyr::tibble(
          x = 0.5,      xend = .x - 0.5,
          y = .y - 0.5, yend = y
        )
      }),
      up = purrr::map2(wday_name, week_year, ~ {
        # n/a if month changes on first day
        if (.x == 1) return(dplyr::tibble(.missing = NA))
        dplyr::tibble(
          x = .x - 0.5, xend = x,
          y = .y - 0.5, yend = .y + 0.5
        )
      }),
      right = purrr::map2(wday_name, week_year, ~ {
        dplyr::tibble(
          x = .x - 0.5, xend = 7.5,
          y = .y + 0.5, yend = y
        )
      })
    )
}


ensure_highlight_day_cols <- function(days, no_fill_first_day = TRUE) {
  stopifnot("day" %in% names(days))
  if (inherits(days$day, "character")) {
    days$day <- lubridate::ymd(days$day)
  }
  for (col in c("label", "color", "fill")) {
    if (!col %in% names(days)) {
      days[[col]] <- NA
    }
  }
  if (no_fill_first_day) {
    days[lubridate::day(days$day) == 1, "fill"] <- NA
  }
  days
}



#' U.S. Federal Holidays
#'
#' Federal Holidays from <https://opm.gov>
#'
#' @references <https://www.opm.gov/policy-data-oversight/snow-dismissal-procedures/federal-holidays>
#'
#' @format A data frame with 100 rows and 5 variables:
#' \describe{
#'   \item{\code{year}}{character. Year}
#'   \item{\code{day}}{double. Date}
#'   \item{\code{label}}{character. Holiday label}
#'   \item{\code{color}}{character. Color for text}
#'   \item{\code{fill}}{character. Fill color}
#' }
"us_federal_holidays"
