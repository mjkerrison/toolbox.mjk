
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
