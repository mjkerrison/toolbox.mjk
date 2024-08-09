
#' Set the names on a vector of paths to those paths' basenames
#'
#' @param x Vector of paths
#'
#' @return Named vector of paths, using basename
#'
#' @description This is very useful for passing lists of paths into \{purrr\}
#'   functions like `map`, which uses the names of the input list \(or the input
#'   list itself when it's an unnamed character vector\) as the names for the
#'   output list.
#'
#' @export
#'
#' @examples
name_path_with_basename <- function(x){
  
  set_names(x, basename(x))
  
}
