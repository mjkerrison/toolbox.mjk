
#' Symmetric set difference.
#'
#' @param a
#' @param b
#'
#' @description Base setdiff returns asymmetric differences: elements in x not
#'   in y. Sometimes you want the more natural set difference - the symmetric
#'   one - where we're looking for *all* elements *not* in *both* sets.
#'
#' @return
#' @export
#'
#' @examples
symmetric_setdiff <- function(a, b){
  
  # Slightly faster than:
  #   setdiff(union(a, b), intersect(a, b))
  # Though that feels like the more natural phrasing.
  
  unique(
    c(
      setdiff(a, b),
      setdiff(b, a)
    )
  )
  
}
