
#' Wrapper around \{officer\} slide creation.
#'
#' @param pack_in The pack to be added to
#' @param .layout The layout to use for this slide
#' @param .master The slide master the layout is in
#' @param ... Name / value pairs, where the name is the placeholder you're
#'   targeting and the value is the object to attempt to print.
#'
#' @description A nicer wrapper for quickly adding a variety of common objects
#'   to possibly complex slide layouts. This is a more flexible approach than
#'   baking in any placeholder names in the function, but still works for that
#'   approach. Main drawback is there's a bit less guidance in the function
#'   formals of what a user should be passing in.
#'   
#'   Pairs well: `officer::plot_layout_properties(pack_in, .layout, .master)`
#'
#' @return
#' @export
#'
#' @examples
add_to_slide <- function(pack_in,
                         .layout,
                         .master,
                         ...){
  
  things_to_output <- list(...)
  
  pack_with_new_slide <- pack_in |>
    
    officer::add_slide(
      layout = .layout,
      master = .master
    )
  
  # TODO: test that
  #   - All names of things_to_output %in% placeholder names for slide layout
  #   - All classes of things_to_output are supported
  
  transformations <- list(
    
    # This is priority order - will pick higher-up method first
    
    "ggplot"    = \(x) rvg::dml(ggobj = x),
    "flextable" = identity,
    "gtsummary" = \(x) gtsummary::as_flex_table(x),
    "tbl"       = \(x) flextable::as_flextable(x),
    "character" = identity
    
  )
  
  
  things_to_output <- things_to_output |>
    
    map(function(thing_i){
      
      available_transformations <- which(names(transformations) %in% class(thing_i))
      
      assert_that(
        length(available_transformations) > 0,
        msg = glue("No method supported for ({paste(class(thing_i), collapse = ' / ')})")
      )
      
      preferred_transformation <- transformations[[first(available_transformations)]]
      
      return(preferred_transformation(thing_i))
      
    })
  
  
  reduce2(
    
    .x = names(things_to_output),
    .y = things_to_output,
    
    .init = pack_with_new_slide,
    
    .f = function(pack_so_far, name_i, thing_i){
      
      pack_so_far |>
        
        officer::ph_with(
          value = thing_i,
          location = officer::ph_location_label(name_i)
        )
      
    }
    
  )
  
  
}
