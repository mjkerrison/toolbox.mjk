
# TODO: integrate with autowrangling.

#' Take a list of tables, and check variable cross-over.
#'
#' @description This is something I write and re-write often - when I have a
#'   bunch of different files that should line up in some way, I want a quick
#'   way to check how well they line up. This is in some ways the backbone for
#'   "autowrangling" - watch this space.
#'
#' @param named_list_of_tables
#' @param force_clean_names
#'
#' @return
#' @export
#'
#' @examples
generate_variable_bus_matrix <- function(named_list_of_tables,
                                         force_clean_names = TRUE){

  assert_that(
    !is.null(names(named_list_of_tables)),
    msg = "List of tables should be named to identify which variables come from where!"
  )


  all_variables <- named_list_of_tables |>

    map(function(x){

      x_names <- names(x)

      if(force_clean_names){
        x_names <- janitor::make_clean_names(x_names)
      }

      tibble(columns = x_names) |>

        mutate(column_position = 1:n())

    })


  bus_matrix <- reduce2(

    .x = all_variables,
    .y = names(all_variables),

    .f = \(accumulated, tbl_i, name_i){

      full_join(
        accumulated,
        tbl_i |> rename({{ name_i }} := column_position),
        by = "columns",
        keep = FALSE
      )

    },

    .init = all_variables |>
      bind_rows() |>
      select(-column_position) |>
      distinct()

  )


  # Check for disjoints:

  bus_matrix_unions <- bus_matrix |> drop_na() |> mutate(Disjoint = FALSE)

  bus_matrix_with_disjoint_info <- bus_matrix |>

    left_join(bus_matrix_unions, by = names(bus_matrix)) |>

    replace_na(list(Disjoint = TRUE))


  return(bus_matrix_with_disjoint_info)

}

