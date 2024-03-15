
#' Fetch a filepath from a synced Teams folder.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @description Still settling on the best way to do this, but - a small
#'   function to retrieve the full path to a file in a synced Teams folder -
#'   maybe buried in a few subfolders. Provides more descriptive errors for
#'   different failure modes.
#'
#'   This can be called two ways: with nothing specified, it will return details
#'   of the Teams folder structure, else it will attempt to return 1 or more
#'   filepaths to files within the Teams folder.
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
    base_path = NULL,
    subfolder = NULL,
    file = NULL,
    like = NULL
  ){
  
  # Dependencies ===============================================================
  # Provide useful error messages if dependencies are not available
  # (Note that we do this so laboriously because we expect this code to be
  # called as raw source code by junior team members)
  
  if(!requireNamespace("assertthat")) stop("Please install package {assertthat}.")
  assert_that <- assertthat::assert_that
  
  if(!requireNamespace("glue"))       stop("Please install package {glue}.")
  glue <- glue::glue
  
  # Getting path ===============================================================
  
  # If `base_path` is NULL, try looking up an environment variable:
  base_path <- if(!is.null(base_path)) base_path else Sys.getenv("TEAMS_PATH")
  
  assert_that(
    base_path != "",
    msg = "`base_path` not specified and `Sys.getenv('TEAMS_PATH')` not defined!"
  )
  
  # Make sure that exists (i.e. is spelled right, is synced):
  assert_that(
    dir.exists(base_path),
    msg = glue("Couldn't find synced folder @ {base_path}")
  )
  
  # If `subfolder` is NULL BUT `file`/`like` *are* specified, throw an error -
  # this pattern suggests a user may have made an error.
  assert_that(
    is.null(subfolder) | (is.null(file) && is.null(like)),
    msg = "`subfolder` not specified, but `file` or `like` was specified. Did you mean to run with no arguments or were you trying to fetch a file?"
  )
  
  # At this point, branch:
  
  ## If nothing provided, explore ----------------------------------------------
  
  if(is.null(subfolder)){
    
    fs::dir_tree(base_path)
    
    return(invisible(NULL))
    
  }
  
  ## Otherwise, try to fetch file(s) -------------------------------------------
  
  full_path <- glue("{base_path}/{subfolder}")

  # Now that we've gotten more specific, make sure *that* input exists as well
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
