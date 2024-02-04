
#' Sort list by name.
#'
#' @description Primarily required for native pipes, which don't support doing
#'   something like list %>% \{.\[sort\(names\(.\)\)\]\} - which, to be fair, is
#'   ugly even when it actually is possible.
#'
#' @param named_list
#'
#' @return named_list, sorted alphabetically by name.
#' @export
#'
#' @examples
sort_list_by_name <- function(named_list){
  
  named_list[sort(names(named_list))]
  
}
