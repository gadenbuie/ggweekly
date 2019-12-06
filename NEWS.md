# ggweekly 0.0.5

* Added `font_base_family` parameter to set the base font family in the
  internal call to `ggplot2::theme_minimal()` (thanks @jbryer #2).
* Renamed `font_label_text` to `font_label_family` for consistency, but the old
  argument isn't completely removed yet. Instead a deprecation warning is issued.

# ggweekly 0.0.4

* Added `week_start` parameter to choose between `isoweek` (Monday) or
  `epiweek` (Sunday) (thanks @tgerke #1)
* Added `weekend_fill` to set or remove the fill used for weekend days (@tgerke #1)
* Set week labels with `week_start_label = "week"` instead of `"isoweek"`
