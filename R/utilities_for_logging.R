
#' Overriding progress::progress_bar defaults
#'
#' @description This is particularly useful when using \{targets\}, where the
#'   console gets a bit fussy.
#'
#' @param n_ticks
#'
#' @return
#' @export
#'
#' @examples
progress_bar <- function(n_ticks){
  
  progress::progress_bar$new(
    format = "(:spin) [:bar] :percent",
    total = n_ticks,
    show_after = 0
  )
  
}

# Otherwise - recommend using {logger} in basically its entirety 
#   - Including for capturing message() and warning() calls
#   - appender_tee() to output to console + log.txt
