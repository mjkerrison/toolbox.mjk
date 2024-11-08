
test_data <- tibble::tribble(
  ~id, ~age, ~label,
   1L,  72L,    "A",
   2L,  15L,    "A",
   3L,   6L,    "B",
   4L,  61L,    "A",
   5L,  41L,    "A"
)

# Option 1 ---------------------------------------------------------------------

age_brackets <- c("0-10",
                  "11-20",
                  "21-60",
                  "61-120")

age_brackets_table <- tibble::tibble(
  age_bracket = age_brackets,
  age_lower = stringr::str_extract(age_brackets, "^.*(?=-)") |> as.numeric(),
  age_upper = stringr::str_extract(age_brackets, "(?<=-).*$") |> as.numeric()
)

dplyr::left_join(
  test_data,
  age_brackets_table,
  by = dplyr::join_by(between(x = age,
                              y_lower = age_lower,
                              y_upper = age_upper,
                              bounds = "[]"))
)

# Option 2 ---------------------------------------------------------------------

# I think overall I'd prefer to just have the table in the code fully:

age_brackets_table <- tibble::tribble(
  ~age_bracket, ~age_lower, ~age_upper,
        "0-10",          0,         10,
       "11-20",         11,         20,
       "21-60",         21,         60,
      "61-120",         61,        120
  )

# I think this is just more transparent and easier to troubleshoot, and requires
# less supporting infrastructure...

dplyr::left_join(
  test_data,
  age_brackets_table,
  by = dplyr::join_by(between(x = age,
                              y_lower = age_lower,
                              y_upper = age_upper,
                              bounds = "[]"))
)

# Nice-to-have -----------------------------------------------------------------

# - Validation of brackets and bounds: do we have MECE coverage?
