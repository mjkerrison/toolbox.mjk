
library(tidyverse)

library(togglr)



set_toggl_api_token(ask_toggl_api_token())


raw_data <- get_time_entries(
  since = Sys.time() - lubridate::weeks(1),
  until = Sys.time()
)

# TODO: snapping to week starts/ends
#   TODO: break-over-month support

# TODO: Figure out what to google to find, or else create:
#   How to identify a timezone based on the time it is there vs the time in AEDT
#   I.e. fingerprint a timezone
#   Basically should just be brute force - but couldn't find any websites that
#     just let you put in an arbitrary time and show you the clocks across many
#     timezones.
#   Everything's built around a different use-case!

custom_rounding <- function(x, round_to = 0.5, threshold = 0.15){

  whole <- floor(x / round_to)

  remainder <- x %% round_to

  n <- whole + (remainder >= threshold)

  return(n * round_to)

}


raw_data |>

  transmute(

    start = start,
    stop = stop,
    date = lubridate::as_date(start), # TODO: support for entries that break over days...
    n_hours = as.numeric(stop - start, units = "hours"),
    project_name = project_name

  ) |>

  group_by(
    date, project_name
  ) |>

  summarise(

    n_hours_sum = sum(n_hours, na.rm = TRUE),

    # Apply rounding *after* aggregation - more favourable to org
    n_hours_rounded = custom_rounding(n_hours_sum)

  ) |>

  ungroup() |>

  select(-n_hours_sum) |> # Only kept this for diagnostics

  arrange(
    date, project_name
  ) |>

  pivot_wider(
    names_from = date,
    values_from = n_hours_rounded
  ) |>

  identity()
