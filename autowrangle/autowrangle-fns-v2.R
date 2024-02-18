
#' Dimensionally model column names across input objects
#'
#' @param named_list_of_tbl_objs 
#'
#' @return
#' @export
#'
#' @examples
fetch_columns <- function(named_list_of_tbl_objs){
  
  assert_that(
    !is.null(names(named_list_of_tbl_objs)),
    msg = "No names provided (to use as source of table)!"
  )
  
  assert_that(
    # Reserve a "source" name for the output of Autowrangling:
    !("Autowrangle Output" %in% names(named_list_of_tbl_objs)),
    msg = "'Autowrangle Output' detected in input sources - please rename input list!"
  )
  
  
  map2(
    named_list_of_tbl_objs,
    names(named_list_of_tbl_objs),
    function(tbl_i, name_i){
      
      tibble(
        
        source = name_i,
        
        colnames_raw = names(tbl_i),
        
        colnames_janitor = janitor::make_clean_names(colnames_raw),
        
        position = 1:length(colnames_raw),
        
        id = map_chr(paste0(source, colnames_janitor), function(src_nm){
          digest::digest(src_nm, algo = "crc32")
        })
        
      )
      
    }
  ) |> bind_rows()
  
}


#' Based on some initial columns, guess how they might connect.
#'
#' @description Consolidate any logic/algorithm to guessing - based on a set of
#'   inputs - how the columns might all align. We probably only end up running
#'   this ~once: past a certain point, we'll be relying on work done manually,
#'   so it's important we don't blend these steps.
#'
#' @param d_columns Column table
#'
#' @return
#' @export
#'
#' @examples
guess_columns <- function(d_columns){
  
  d_columns |> 
    
    identity()
  
}


