
# TODO: punch this up - weirdly tied to being a column.

#' Check if something's a dttm object
#'
#' @description I ended up needing this specifically for a variety of things to
#'   do with reading date columns in and out of SQLite databases - but it comes
#'   up elsewhere too.
#'
#' @param tibble_col
#'
#' @return
#' @export
#'
#' @examples
is_dttm <- function(tibble_col){
  
  all(class(tibble_col) == c("POSIXct", "POSIXt"))
  
}



#' Get the End of Financial Year date relevant to a given date.
#'
#' @param date_in 
#' @param .eofy To support any non-Australians.
#'
#' @return
#' @export
#' 
#' @importFrom lubridate dmy
#' @importFrom lubridate quarter
#'
#' @examples
get_EOFY <- function(date_in, .eofy = "30/6"){
  
  lubridate::dmy(
    paste0(
      .eofy,
      "/",
      floor(lubridate::quarter(date_in, with_year = T, fiscal_start = 7))
    )
  )
  
}



#' Get the Start of Financial Year date relevant to a given date.
#'
#' @param date_in 
#' @param .sofy To support any non-Australians.
#'
#' @return
#' @export
#'
#' @importFrom lubridate dmy
#' @importFrom lubridate quarter
#'
#' @examples
get_SOFY <- function(date_in, .sofy = "1/7"){
  
  lubridate::dmy(
    paste0(
      .sofy,
      "/",
      floor(lubridate::quarter(date_in, with_year = T, fiscal_start = 7)) - 1
    )
  )
  
}
