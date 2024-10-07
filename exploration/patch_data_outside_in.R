

# Patching data from the outer layer in, monitoring what gets kept/discarded
# where:

#' Title
#'
#' @param non_id_cols
#' @param list_of_demand_extracts List of tables that represent multiple demand
#'   extracts, ORDERED from MOST preferred to LEASE preferred \(i.e. last patch
#'   first, then older patches IN ORDER, then the base data. Imagine stacking
#'   Swiss cheese: you want the favourite slice at the top so you "see" it, and
#'   you only see less-relevant slices through the holes.\)
#'
#' @return
#' @export
#'
#' @examples
collate_multiple_demand_extracts <- function(non_id_cols = c("total_sum_unit"),
                                             list_of_demand_extracts,
                                             dev_version = FALSE){
  
  assert_that(length(list_of_demand_extracts) >= 2,
              msg = "2 or more demand extracts required.")
  
  
  results <- reduce(
    
    # Everything but the first...
    .x = tail(list_of_demand_extracts, -1),
    
    # ...Because we'll use that as our starting point (final 'slice').
    .init = list(
      list(
        kept = list_of_demand_extracts[[1]],
        discarded = tibble()
      )
    ),
    
    function(list_of_results_so_far, extract_i){
      
      # Compile our data so far...
      data_so_far <- map(list_of_results_so_far, pluck("kept")) |> bind_rows()
      
      # Ensure columns are consistent across demand extracts - but order can
      # be different.
      assert_that(
        identical(
          sort(names(data_so_far)), 
          sort(names(extract_i))
        ),
        msg = "Column names must be identical across demand extracts."
      )
      
      # Anti-join by all columns (shared - see above), assuming all columns are
      # "ID" columns, EXCEPT any specified in non_id_cols (which should be
      # measures, i.e. total_sum_unit or similar.) (Plus a manually-added
      # diagnostic column - so we can track which rows came from where.)
      joining_by <- setdiff(names(data_so_far), non_id_cols)
      
      
      # Stuff in our less-preferred data that isn't covered already:
      to_keep <- anti_join(
        extract_i, # Anything in X
        data_so_far, # Not in Y
        by = joining_by # By shared stuff - see above 
      )
      
      to_discard <- anti_join(
        extract_i, # Anything in X
        to_keep, # Not in Y
        by = joining_by # By shared stuff - see above 
      )
      
      return(
        c(
          list_of_results_so_far,
          list(
            list(
              kept = to_keep,
              discarded = to_discard
            )
          )
        )
      )
      
      
    }
    
  )
  
  
  # If dev, return this list-of-lists so we can see what's getting
  # kept/discarded from where:
  if(dev_version) return(results)
  
  # Otherwise, in normal use? Just pluck the kept rows and bind them.
  map(results, pluck("kept")) |> bind_rows()
  
}
