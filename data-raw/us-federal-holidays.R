library(dplyr)
library(rvest)
library(tidyr)
library(lubridate)

opm <- read_html("https://www.opm.gov/policy-data-oversight/snow-dismissal-procedures/federal-holidays/#url=2020")

opm_years <- opm %>% html_nodes("caption") %>% html_text()

us_federal_holidays <-
  opm %>%
  html_nodes(".DataTable") %>%
  html_table() %>%
  set_names(opm_years) %>%
  map_dfr(~ ., .id = "caption") %>%
  rename(day = Date, label = Holiday) %>%
  extract(caption, "year", "(\\d{4})") %>%
  mutate(
    day = gsub("[^a-zA-Z0-9 ]", "", day),
    day = case_when(
      !grepl("\\d{4}", day) ~ paste(day, year),
      TRUE ~ day
    ),
    day = parse_date_time(day, c("A B d Y")),
    day = as_date(day),
    color = "#00724a",
    fill = "#00724a"
  )

usethis::use_data(us_federal_holidays, overwrite = TRUE)
