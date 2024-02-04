
# Compound inequalities ========================================================

#' Infix operators for compound inequalities
#'
#' @description Kudos to http://jdobr.es/blog/compound-inequalities-r/! Not
#'   exported. Lets you do things like x %<=% y %<=% z.
#'
#' @param lhs
#' @param rhs
#' @param comparison
#'
#' @return
#'
#' @examples
compound.inequality <- function(lhs, rhs, comparison) {
  if (is.null(attr(lhs, 'compound-inequality-partial'))) {
    out <- rhs
    attr(out, 'compound-inequality-partial') <- do.call(comparison, list(lhs, rhs))
  } else {
    out <- do.call(comparison, list(lhs, rhs)) & attr(lhs, 'compound-inequality-partial')
  }
  
  return(out)
}



#' Strictly Less Than
#'
#' @param lhs 
#' @param rhs 
#'
#' @return lhs < rhs ? TRUE : FALSE
#' @export
#'
#' @examples
'%<<%' <- function(lhs, rhs) {
  return(compound.inequality(lhs, rhs, '<'))
}



#' Less Than Or Equal To
#'
#' @param lhs 
#' @param rhs 
#'
#' @return lhs <= rhs ? TRUE : FALSE
#' @export
#'
#' @examples
'%<=%' <- function(lhs, rhs) {
  return(compound.inequality(lhs, rhs, '<='))
}



#' Strictly Greater Than
#'
#' @param lhs 
#' @param rhs 
#'
#' @return lhs > rhs ? TRUE : FALSE
#' @export
#'
#' @examples
'%>>%' <- function(lhs, rhs) {
  return(compound.inequality(lhs, rhs, '>'))
}



#' Greater Than Or Equal To
#'
#' @param lhs 
#' @param rhs 
#'
#' @return lhs >= rhs ? TRUE : FALSE
#' @export
#'
#' @examples
'%>=%' <- function(lhs, rhs) {
  return(compound.inequality(lhs, rhs, '>='))
}


# Equality with NAs ============================================================


#' Inequality, treating NAs as a Thing
#'
#' @description Courtesy of https://stackoverflow.com/questions/37610056.
#'
#' @param e1 
#' @param e2 
#'
#' @return
#' @export
#'
#' @examples
`%!=na%` <- function(e1, e2){
  (
    e1 != e2 |
      (is.na(e1) & !is.na(e2)) |
      (is.na(e2) & !is.na(e1))
  ) & !(is.na(e1) & is.na(e2))
}


#' Equality, treating NAs as a Thing
#'
#' @param e1 
#' @param e2 
#'
#' @return
#' @export
#'
#' @examples
`%==na%` <- function(e1, e2){!`%!=na%`(e1, e2)}

