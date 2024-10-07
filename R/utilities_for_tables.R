
#' bind_rows, but track what rows came from where
#'
#' @param named_list_of_tables A list of tables with names we can use as a
#'   'source' column.
#' @param ... Passed on to bind_rows.
#'
#' @return
#' @export
#'
#' @examples
bind_rows_with_source <- function(named_list_of_tables, ...){
  
  map2(
    named_list_of_tables,
    names(named_list_of_tables),
    function(tbl_i, name_i){
      mutate(tbl_i, source = name_i, .before = 1)
    }
  ) |>
    
    bind_rows(...)
  
}


#' coalesce\(\) across columns
#'
#' @description Taken from https://github.com/tidyverse/funs/issues/54
#'
#' @param ... tidyselection - should be called within a tidyselect context
#'
#' @return
#' @export
#'
#' @examples
coalesce_across <- function(...){
  coalesce(!!!across(...))
}


#' Convert columns to a 'lookup' or dict
#'
#' @param table The table containing a key column \(position 1\) and  a value
#'   column \(position 2\).
#'
#' @return A named vector - generated via rlang::set_names
#' @export
#'
#' @examples
as.dict <- function(table){
  
  assert_that(
    ncol(table) == 2,
    msg = "Expected only two columns!"
  )
  
  set_names(
    x  = table[[2]], # The values, retrieved up via...
    nm = table[[1]]  # ...the names.
    # NB this way (x[[...]]) should be robust for tibbles, data.frames, ...
  )
  
}


#' Check names are present in table
#'
#' @param table_i Table to verify
#' @param names_to_check Character vector of names to look for
#'
#' @return The input table, if it passes the check
#'
#' @description IMO this is more natural than how you handle this in e.g. the
#'   \{assertr\} package.
#'
#' @export
#'
#' @examples
assert_has_all_names <- function(table_i, names_to_check){
  
  validation_vector <- names_to_check %in% names(table_i)
  
  assert_that(
    all(validation_vector),
    msg = glue(
      "Not all required names found in table: missing {missing_names}",
      missing_names = names_to_check[!validation_vector]
    )
  )
  
  return(table_i)
  
}


#' Efficiently dplyr::rename with pre-defined concordance
#'
#' @param table_i Table to rename
#' @param new_names The new names, pairwise with...
#' @param old_names ...the old names
#'
#' @return Table with renamed columns
#'
#' @description You could achieve this using dplyr::rename and the custom
#'   as.dict function directly, but this is IMO a clearer wrapper around the
#'   same idea.
#'
#' @export
#'
#' @examples
rename_using <- function(table_i, new_names, old_names){
  
  assert_that(
    ncol(table_i) == length(new_names) & ncol(table_i) == length(old_names),
    msg = "Input table, `new_names` and `old_names` have differing lengths."
  )
  
  rename(
    table_i,
    !!!set_names(x = old_names,
                 nm = new_names)
  )
  
}

