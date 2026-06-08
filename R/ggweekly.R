#' Create a Weekly Planner
#'
#' Creates a weekly calendar with ggplot2
#'
#' @param start_day The starting day, either as a YYYY-mm-dd text string or
#'   using [lubridate::ymd()].
#' @param end_day The ending day, either as a YYYY-mm-dd text string or
#'   using [lubridate::ymd()].
#' @param highlight_days Special days to be highlighted, formatted as a tibble
#' with columns `day`, `label`, `color`, and `fill`.
#' @param week_start Should the week start on Monday (`"isoweek"`) or on Sunday
#'   (`"epiweek"`)?
#' @param week_start_label What labels should be used for the week start, i.e.
#'   to the left of the first day of the week? One of `"month day"`, `"week"`,
#'   or `"none"`.
#' @param show_day_numbers Should day numbers be included in each box of the
#'   calendar?
#' @param show_month_start_day Should the first day of the month be highlighted?
#' @param show_month_boundaries Should boundaries between months be highlighted
#'   with a dotted line?
#' @param highlight_text_size Size of the text in the highlighted days.
#' @param month_text_size The size of the text labelling the first day of the month.
#' @param day_number_text_size The size of the text labelling day number.
#' @param month_color The color of the boxes highlighting the first day of the month.
#' @param day_number_color The color of the day number text.
#' @param weekend_fill The color to fill the weekend days.
#' @param font_base_family Base font passed to `base_family` in
#'   \code{\link{ggplot2::theme_minimal}}, default is "PT Sans".
#' @param holidays A tibble containing holiday dates in the same format as
#'   `higlight_days`. Defaults to a list of US Federal Holidays. Set to `NULL`
#'   to disable.
#' @param font_label_family,font_label_text Font for label text, default is
#'   [PT Sans Narrow](https://fonts.google.com/specimen/PT+Sans+Narrow). Note
#'   that `font_label_text` is deprecated in favor of `font_label_family`.
#'
#' @return A ggplot2 object with the weekly calendar plot.
#'
#' @examples
#' \dontrun{
#' # Create a weekly planner
#' ggweek_planner(
#'   start_day = "2019-04-01",
#'   end_day = "2019-06-30",
#' )
#'
#' # Create a standard calendar
#' ggweek_planner(
#'   start_day = "2019-04-01",
#'   end_day = "2019-06-30",
#'   show_month_boundaries = FALSE,
#'   show_month_start_day = FALSE,
#'   week_start = "isoweek",
#'   week_start_label = "week"
#' ) +
#'   ggplot2::ggtitle("2019") +
#'   ggplot2::facet_wrap(~ month, scales = 'free')
#' }
#'
#' @importFrom rlang %||%
#' @export
ggweek_planner <- function(
  start_day = lubridate::today(),
  end_day = start_day + lubridate::weeks(8) - lubridate::days(1),
  highlight_days = NULL,
  week_start = c("isoweek", "epiweek"),
  week_start_label = c("month day", "week", "none"),
  show_day_numbers = TRUE,
  show_month_start_day = TRUE,
  show_month_boundaries = TRUE,
  highlight_text_size = 2,
  month_text_size = 4,
  day_number_text_size = 2,
  month_color = "#f78154",
  day_number_color = "grey80",
  weekend_fill = "#f8f8f8",
  holidays = ggweekly::us_federal_holidays,
  font_base_family = "PT Sans",
  font_label_family = "PT Sans Narrow",
  font_label_text = NULL
) {
  week_start <- match.arg(week_start)
  if (week_start == "epiweek") {
    old_opts <- options("lubridate.week.start" = 7)
    get_week <- lubridate::epiweek
    get_year <- lubridate::epiyear
  } else {
    old_opts <- options("lubridate.week.start" = 1)
    get_week <- lubridate::isoweek
    get_year <- lubridate::isoyear
  }
  on.exit(options(old_opts))

  if (!is.null(font_label_text)) {
    font_label_family <- font_label_text
    warning(
      "The `font_label_text` argument has been renamed `font_label_family` ",
      "and will be removed in a future release of {ggweekly}."
    )
  }

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
      wday_name = lubridate::wday(.data$day, label = TRUE, abbr = TRUE),
      weekend   = lubridate::wday(.data$day, label = FALSE, week_start = 1) %in% 6:7,
      week      = get_week(.data$day),
      month     = lubridate::month(.data$day, label = TRUE, abbr = FALSE),
      year      = get_year(.data$day)
    )

  dates <- dates %>%
    dplyr::mutate(
      week_year = sprintf("%s - %s", .data$year, .data$week),
      week_year = forcats::fct_inorder(.data$week_year),
      week_year = forcats::fct_rev(.data$week_year)
    )

  day_one <- dates %>%
    dplyr::filter(lubridate::day(.data$day) == 1)

  weekend_fill <- weekend_fill %||% "#FFFFFF"

  gcal <-
    dates %>%
    dplyr::mutate(
      # Softly fill in the weekend days
      weekend = dplyr::if_else(.data$weekend, weekend_fill, "#FFFFFF")
    ) %>%
    dplyr::arrange(.data$day) %>%
    ggplot2::ggplot() +
    ggplot2::aes(.data$wday_name, .data$week_year) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$weekend), color = "#aaaaaa") +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_color_identity() +
    ggplot2::guides(fill = "none") +
    ggplot2::theme_minimal(base_family = font_base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5),
      axis.text = ggplot2::element_text(family = font_label_family),
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
  } else if (week_start_label == "week") {
    gcal <- gcal +
      ggplot2::scale_y_discrete(
        breaks = levels(dates$week_year),
        labels = sub("^\\d{4} - ", "", levels(dates$week_year))
      )
  }

  if (show_day_numbers) {
    gcal <- gcal +
      ggplot2::geom_text(
        ggplot2::aes(label = lubridate::day(.data$day)),
        family = font_label_family,
        color = day_number_color,
        size = day_number_text_size,
        hjust = 1,
        nudge_x = 0.45,
        vjust = 1,
        nudge_y = 0.45
      )
  }

  if (show_month_start_day && nrow(day_one)) {
    gcal <- gcal +
      ggplot2::geom_tile(
        data = day_one,
        fill = month_color,
        alpha = 0.25
      ) +
      ggplot2::geom_text(
        data = day_one,
        ggplot2::aes(label = .data$month),
        family = font_label_family,
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
        ggplot2::aes(fill = .data$fill),
        color = NA,
        alpha = 0.25
      ) +
      ggplot2::geom_text(
        data = dates %>% dplyr::inner_join(holidays, by = "day"),
        ggplot2::aes(label = .data$label, color = .data$color),
        family = font_label_family,
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
        ggplot2::aes(fill = .data$fill),
        color = NA,
        alpha = 0.25
      ) +
      ggplot2::geom_text(
        data = dates %>% dplyr::inner_join(highlight_days, by = "day"),
        ggplot2::aes(label = .data$label, color = .data$color),
        family = font_label_family,
        size = highlight_text_size,
        hjust = 0,
        nudge_x = -0.45,
        vjust = 0,
        nudge_y = -0.40
      )
  }

  if (show_month_boundaries && nrow(day_one)) {
    month_boundaries <- day_one %>%
      generate_month_boundaries()

    for (boundary in c("left", "up", "right")) {
      gcal <- gcal +
        ggplot2::geom_segment(
          data = month_boundaries %>% tidyr::unnest(!!rlang::sym(boundary)),
          ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend),
          color = "#999999",
          linetype = 2
        )
    }
  }

  gcal
}

week_start_labels <- function(
  dates,
  week_start = getOption("lubridate.week.start", 1)
) {
  dates %>%
    dplyr::arrange(.data$day) %>%
    dplyr::group_by(.data$week_year) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$day) %>%
    dplyr::mutate(
      day = lubridate::floor_date(.data$day, "week", week_start = week_start),
      month = lubridate::month(.data$day, label = TRUE),
      label = dplyr::case_when(
        month == dplyr::lag(.data$month) ~ paste(lubridate::day(.data$day)),
        TRUE ~ sprintf("%s %4i", .data$month, lubridate::day(.data$day))
      )
    ) %>%
    dplyr::select("label", "week_year") %>%
    purrr::reduce(purrr::set_names)
}

generate_month_boundaries <- function(dates) {
  dates %>%
    dplyr::filter(lubridate::day(.data$day) == 1) %>%
    dplyr::select(.data$day, .data$month, .data$wday_name, .data$week_year) %>%
    dplyr::mutate_at(dplyr::vars(.data$wday_name, .data$week_year), as.integer) %>%
    dplyr::mutate(
      left = purrr::map2(.data$wday_name, .data$week_year, ~ {
        # n/a if month changes on first day
        if (.x == 1) return(dplyr::tibble(.missing = NA))
        dplyr::tibble(
          x = 0.5,      xend = .x - 0.5,
          y = .y - 0.5, yend = .data$y
        )
      }),
      up = purrr::map2(.data$wday_name, .data$week_year, ~ {
        # n/a if month changes on first day
        if (.x == 1) return(dplyr::tibble(.missing = NA))
        dplyr::tibble(
          x = .x - 0.5, xend = .data$x,
          y = .y - 0.5, yend = .y + 0.5
        )
      }),
      right = purrr::map2(.data$wday_name, .data$week_year, ~ {
        dplyr::tibble(
          x = .x - 0.5, xend = 7.5,
          y = .y + 0.5, yend = .data$y
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
