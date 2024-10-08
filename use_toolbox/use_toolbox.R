#' Download one or more files from mjkerrison's GitHub toolbox
#'
#' @param scripts The list of scripts to grab
#'
#' @description Downloads the files specified from the latest version of the
#'   main branch, and writes them to the R/ folder of the current project. Will
#'   support files not in R/ in the repo, but will write them all to the
#'   project's R/ folder, and we have a pre-populated list of all R/ files below
#'   the function definition.
#'
#' @return
#' @export
#'
#' @examples
download_from_github <- function(scripts,
                                 branch = "main"){
  download.file(
    url = glue("https://raw.githubusercontent.com/mjkerrison/toolbox.mjk/{branch}/{scripts}"),
    destfile = glue("R/{basename(scripts)}")
  )
}

# This section is updated via GitHub Actions for convenience when commits to the
# main branch add new files to the R/ folder
download_from_github(c(
  'R/dump_list_of_tables_to_sheet.R',
  'R/fetch_from_teams.R',
  'R/more_infix_operators.R',
  'R/utilities_for_databases.R',
  'R/utilities_for_dates.R',
  'R/utilities_for_Excel.R',
  'R/utilities_for_logging.R',
  'R/utilities_for_structures.R',
  'R/utilities_for_tables.R',
  'R/variables-bus-matrix.R',
))

# TODO: fix this. This is way too wacky!! But it was good GitHub CI practice.
