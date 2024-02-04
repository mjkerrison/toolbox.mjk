
# TODO:
#   - Make the fetch_ functions consistent - currently one is per sheet, while
#     the other is all sheets.
#   - Decide whether utilities go in here or separately.
#   - Clean up dependencies


# This might be worth a whole package.


# Depends on R/utilities_for_tables.R


# Reading config ===============================================================

#' Look at a folder of CSVs, get paths, and infer target object names
#'
#' @param path_to_folder
#'
#' @return
#' @export
#'
#' @examples
parse_config_dir <- function(path_to_folder){

  tibble(

    path = path_to_folder |>

      list.files(pattern = "\\.csv$", full.names = TRUE),

    name = path |>

      basename() |>

      str_replace("\\.csv$", "")

  ) |>

    # Make sure this is in {key, value} order...
    relocate(name, path)

}



#' Read a bunch of .csvs with raw and recoded variable values
#'
#' @description Reads CSVs that have 1x "raw" column and 1x "recoded" column
#'   that recode a variable. The target variable name should always be the
#'   filename. This allows us to have very long variable names!
#'
#' @param path_to_folder
#'
#' @return
#' @export
#'
#' @examples
fetch_recoding_tables <- function(path_to_folder,
                                  .encoding = c("Windows-1252", "UTF-8")){

  # Grab first of default args if none supplied
  .encoding <- match.arg(.encoding)


  all_recoding_tbls <- parse_config_dir(path_to_folder) |>

    as.dict() |>

    map(\(x){
      suppressMessages(
        read_csv(
          x,
          locale = locale(encoding = .encoding)
        )
      )
    })


  assert_that(
    all(
      map_lgl(all_recoding_tbls, \(tbl_i){
        all(c("raw", "recoded") %in% names(tbl_i))
      })
    ),
    msg = "At least one variable recoding table is malformed - refer function definition!"
  )


  return(all_recoding_tbls)

}


#' Read 'autowrangling' config .CSVs from a folder
#'
#' @param path_to_folder
#'
#' @return All config tables in the folder. Note we apply some light logic:
#'   blanks are treated as take-no-action - either treated as a FALSE for binary
#'   operations \(dropping, coalescing wide cols\) or use-existing for renaming
#' @export
#'
#' @examples
fetch_autowrangle_config <- function(path_to_folder){

  parse_config_dir(path_to_folder) |>

    as.dict() |>

    map(function(file_i){

      tbl_i <- suppressMessages(read_csv(file_i))

      assert_that(
        all(c("columns", "Drop?", "Coalesce?", "Target column") %in% names(tbl_i)),
        msg = glue::glue("{file_i} is malformed - refer function definition!")
      )

      # TODO: pop in some more checks re: {janitor}ial status of these names
      #   E.g. uniqueness etc.

      tbl_i |>

        # If no new target column is specified, assume old name is retained
        mutate(`Target column` = coalesce(`Target column`, columns)) |>
        # NB that currently this creates a target column for Dropped columns as
        # well - doesn't really matter but does look kinda weird.

        # If Drop or Coalesce have not been specified, assume not (FALSE).
        replace_na(list(
          `Drop?` = FALSE,
          `Coalesce?` = FALSE
        ))

    })

}



# Execution ====================================================================


#' Recoding a variable on LHS using map provided on RHS
#'
#' @description Carved this out from within autowrangle_data_from_config\(\)
#'   because it's a bit too useful in its own right.
#'
#' @param tbl_to_be_recoded LHS - your data
#' @param tbl_to_recode_with RHS - your map of old values \(`raw`\) to new
#'   \(`recoded`\)
#' @param variable_to_recode The name of the variable on LHS that corresponds to
#'   `raw`
#' @param table_identifier Optional - for debugging / warning / traceback
#'
#' @return
#' @export
#'
#' @examples
recode_variable <- function(tbl_to_be_recoded,
                            tbl_to_recode_with,
                            variable_to_recode,
                            table_identifier = NULL){

  # TODO: push this upstream somewhere, probably
  assert_that(
    !("recoded" %in% names(tbl_to_be_recoded)),
    msg = "`recoded` should be a reserved name, but exists in this data table!"
  )


  joined <- tbl_to_be_recoded |>

    left_join(
      tbl_to_recode_with,
      by = set_names(nm = variable_to_recode, x = "raw")
    )


  # If we still had any unexpected values in a given variable, flag that,
  # but continue on. Note that this is permissive of NAs in the original data.
  # TODO: think about how we integrate NA checking in all of this...

  joined_checking <- joined |>

    # Have to switch back to !!sym(...) from {{ }} - I really need to grok
    # *exactly* what {{ }} *actually does*...
    mutate(no_match = !is.na(!!sym(variable_to_recode)) & is.na(recoded))


  if(any(joined_checking$no_match)){

    log_warn(glue::glue(
      "Unexpected values of `{variable_to_recode}` detected in {maybe_null_id}. Coalescing...",
      maybe_null_id = ifelse(is.null(table_identifier), "???", table_identifier)
    ))

  }


  joined |>

    mutate(
      {{ variable_to_recode }} := coalesce_across(all_of(c("recoded", variable_to_recode)))
    ) |>

    select(-recoded)

}




#' Apply config to 'autowrangle' one table
#'
#' @param one_unwrangled_table
#' @param wrangling_config_table
#' @param variable_recoding_tables
#'
#' @return
#' @export
#'
#' @examples
autowrangle_data_from_config <- function(one_unwrangled_table,
                                         wrangling_config_table,
                                         variable_recoding_tables,
                                         table_identifier = NULL){

  # 1 Dropping any columns =====================================================

  log_debug("Dropping...")

  cols_to_drop <- wrangling_config_table |>

    filter(`Drop?` == TRUE) |>

    pull(columns)

  data_pt1_retained <- one_unwrangled_table |>

    select(-any_of(cols_to_drop))


  # 2 Coalescing any columns ===================================================

  log_debug("Coalescing...")

  # NB that this is robust to coalescing_targets being empty i.e. character(0)

  coalescing_targets <- wrangling_config_table |>

    # Make sure you only *attempt* to coalesce columns that *exist* in this
    # particular table...
    filter(
      `Coalesce?` == TRUE,
      columns %in% names(one_unwrangled_table)
    ) |>

    pull(`Target column`) |>

    unique()


  data_pt2_coalesced <- reduce(

    # Reduce over (.x)
    coalescing_targets,

    # Using the function
    function(table_so_far, coalesce_target_i){

      things_to_coalesce <- wrangling_config_table |>

        filter(`Target column` == coalesce_target_i) |>

        pull(columns)

      # For super-debugging:
      log_trace(things_to_coalesce)

      table_so_far |>

        mutate(
          {{ coalesce_target_i }} := coalesce_across(any_of(things_to_coalesce))
        ) |>

        select(-any_of(things_to_coalesce))

    },

    # Starting with
    .init = data_pt1_retained

  )

  # 3 Renaming other columns ===================================================

  log_debug("Renaming...")

  new_name_dict <- wrangling_config_table |>

    filter(`Drop?` == FALSE) |>

    select(columns, `Target column`) |>

    as.dict()


  new_names <- coalesce(

    # Take all the columns we have at this point, and try to find a new name
    # based on the dictionary we compile from the config above.
    new_name_dict[names(data_pt2_coalesced)],

    # But if that's an NA - i.e. it found no lookup - it should be because we
    # already processed that through an intermediate step, and it's *already*
    # named according to `Target column` - so fall back to that.
    names(data_pt2_coalesced)

    # Note also that at this point it's OK that those coalesced values won't
    # themselves have a name.

  )

  # For super-debugging:
  log_trace(new_names)


  data_pt3_renamed <- data_pt2_coalesced |>

    set_names(new_names)


  # 4 Any variable recoding ====================================================

  log_debug("Recoding...")

  relevant_recoding_tbls <- variable_recoding_tables |>

    purrr::keep_at(\(col_i){col_i %in% names(data_pt3_renamed)})


  data_pt4_recoded <- reduce2(

    # Reduce over (.x and .y)
    relevant_recoding_tbls,
    names(relevant_recoding_tbls),

    # Using the function
    function(table_so_far, recoding_tbl_i, variable_i){

      recode_variable(
        table_so_far,
        recoding_tbl_i,
        variable_i,
        table_identifier
      )

    },

    # Starting with
    .init = data_pt3_renamed

  )

  # Return =====================================================================

  log_debug("Success!")

  return(data_pt4_recoded)


}


#' Apply config to 'autowrangle' one or more tables
#'
#' @param unwrangled_input Can be a list or a single table - "dispatches"
#'   appropriately.
#' @param wrangling_config_table
#' @param variable_recoding_tables
#'
#' @return
#' @export
#'
#' @examples
autowrangle <- function(unwrangled_input,
                        wrangling_config_table,
                        variable_recoding_tables){

  assert_that(
    !is.null(wrangling_config_table),
    msg = "Must supply config table!"
  )

  # TODO: make sure this all supports NULL recoding tables.

  # TODO: extend (?) this to

  is_a_list <- identical("list", class(unwrangled_input))

  if(!is_a_list){

    # Do it once

    autowrangle_data_from_config(
      unwrangled_input,
      wrangling_config_table,
      variable_recoding_tables
    )

  } else {

    # Map over the list and do it to each

    map2(
      unwrangled_input,
      names(unwrangled_input),
      function(x, y){
        autowrangle_data_from_config(
          x,
          wrangling_config_table,
          variable_recoding_tables,
          table_identifier = y
        )
      }
    )

  }

}
