
#' Fetch a filepath from a synced Teams folder.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @description Still settling on the best way to do this, but - a small
#'   function to retrieve the full path to a file in a synced Teams folder -
#'   maybe buried in a few subfolders. Provides more descriptive errors for
#'   different failure modes.
#'
#' @param base_path
#' @param subfolder
#' @param file
#' @param like
#'
#' @return
#' @export
#'
#' @examples
fetch_from_teams <- function(
    base_path,
    subfolder,
    file = NULL,
    like = NULL
  ){

  full_path <- glue("{base_path}/{subfolder}")

  # Check that the folder exists (i.e. is spelled right, is synced):
  assert_that(
    dir.exists(full_path),
    msg = glue("Couldn't find synced folder @ {full_path}")
  )

  # Check that the user has specified their "search" properly:
  assert_that(
    #         Not both NULL           &&           Not both specified
    !(is.null(file) && is.null(like)) && !(!is.null(file) && !is.null(like)),
    msg = "Must specify one (and only one) of `file` or `like`!"
  )

  # Escape any special characters in filename, if it's been supplied
  # H/t https://stackoverflow.com/a/14838321
  file <- if(is.null(file)) file else gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", file)

  # Use whichever "search" term has been supplied
  search_string <- coalesce(file, like)

  # Look for files
  found_files <- list.files(
    path = full_path,
    pattern = search_string,
    full.names = TRUE
  )

  # Check we got anything
  assert_that(
    length(found_files) > 0,
    msg = glue("Couldn't find '{search_string}' in {full_path}")
  )

  # Give the list of paths names - specifically, using the filename part. Useful
  # for throwing these lists into map() - so you get intelligible names, not
  # just indices.
  final_result <- set_names(found_files,
                            basename(found_files))

  return(final_result)


}
