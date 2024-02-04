
#' For when you need to dump many tables to the *same* Excel sheet.
#'
#' @description Takes a list of table-like objects, creates a new sheet on an
#'   existing \{openxlsx\} workbook, and slots them in sequentially on the
#'   far-left of the sheet, with a given amount of space between them.
#'
#' @param named_list_of_tables
#' @param openxlsx_workbook
#' @param target_sheet
#' @param .extra_space
#'
#' @return TRUE, invisibly
#' @export
#'
#' @importFrom openxlsx addWorksheet
#' @importFrom openxlsx writeData
#'
#' @examples
dump_list_of_tables_to_sheet <- function(named_list_of_tables,
                                         openxlsx_workbook,
                                         target_sheet,

                                         # +1 @ top for object name
                                         # +1 @ top for headers
                                         # +2 @ bottom for spacing
                                         .extra_space = 4){


  openxlsx::addWorksheet(openxlsx_workbook,
                         sheetName = target_sheet)


  # Running row tally for manual placement of multiple tables in same Excel sheet
  row_placements <- tibble(

    object = names(named_list_of_tables),

    heights = named_list_of_tables |>

      map_dbl(nrow),

    extra_space = .extra_space,

    # +1 for 1-indexing
    end_row = cumsum(heights) + cumsum(extra_space) + 1,

    start_row = dplyr::lag(end_row, n = 1, default = 1)

  )

  row_placement_lookup <- row_placements |>
    select(object, start_row) |>
    as.dict()


  walk2(named_list_of_tables,
        names(named_list_of_tables),
        function(tbl_i, name_i){

          # And don't forget, {openxlsx} is OOP, so we don't need to return
          # objects etc. etc.

          # Write the object name as a header
          openxlsx::writeData(
            wb       = openxlsx_workbook,
            sheet    = target_sheet,
            x        = name_i,
            startRow = row_placement_lookup[name_i]
          )

          # TODO: make this a bit more flexible... ~~~~~~~
          
          # Set some classes for nice format of output in Excel:

          target_pct_cols <- purrr::keep(names(tbl_i), ~str_ends(., "Percent"))

          for(col_i in target_pct_cols){

            class(tbl_i[[col_i]]) <- "percentage"

          }
          
          # End TODO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

          # Write the table - having left room for column headers:
          openxlsx::writeData(
            wb       = openxlsx_workbook,
            sheet    = target_sheet,
            x        = tbl_i,
            startRow = row_placement_lookup[name_i] + 1,
            na.string = "NA"
          )

        })

  return(invisible(TRUE))

}
