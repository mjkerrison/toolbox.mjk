
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FIELD PROMOTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Because our {officer} has done a lot of good work on the battlefield, and so
# deserves to be taken to the next level.


# Thinking =====================================================================
#
# In essence what we're trying to get to is something more like literate
# programming, but using {officer} techniques, which isn't supported per se in
# any of the existing (or at least, standard) Rmd / Qmd frameworks
#
# (Makes sense - this would be, I suppose, a pandoc thing?)
#
# So what if we just... wrote our own minimal version of this? Maybe we could
# write a non-pandoc backend for knitr to use this, so we could use the RStudio
# visual editor...?
# 
# MVP - let's get to having a list-of-lists like:
# 
# list(
# 
#   "One plot RHS panel" = list(
#   
#     plot = plot_XYZ(...),
#     title = "My plot",
#     footer = "Source",
#     commentary = "As you can see..."
#   
#   )
# 
# ) |> render_powerpoint(master = 'Office Default', out = 'my-ppt.pptx')
# 
# ...which should create a one-slide ppt from a template, using the layout 'One
# plot RHS panel', populating the placeholders `plot`, `title`, `footer`, and
# `commentary` with those things.
#   - Doing up 1 slide should be handled by a 'create slide' function
#   - This should also add the UUIDs to this slide - and this functionality
#     needs to be robust to there being multiple objects (so, appending to e.g.
#     speaker notes rather than overwriting wholesale)
# 
# It should output that powerpoint to 'my-ppt.pptx'.
#   - This should be handled by render_powerpoint
# 
# As side-effects, it should create a folder called my-ppt alongside the .pptx
# itself, filling that with the data *immediately behind* each plot.
#   - Or, really, each component of each slide which has a method *for
#     outputting data*, which we'll write - so raw character probably nothing, 
#     but generic ggplots could just grab the `data` object from the ggplot.
#     - But we might be well-served by creating a special class (e.g.
#       'viz_artefact') that has a $viz and a $data slot/item, plus a print
#       method (like ggplots), which could then better reflect any manipulations
#       to the data that occur within the plotting function).
#         - NB: let's stick with "item" rather than "slot" - I think S3 will 
#           suit us better than S4 for our approach; flexibility will be
#           valuable e.g. for creating new methods as we go (e.g. sankeys,
#           HTML widgets, ...)
#
# In terms of checking and error handling, it should:
#   - For a given slide, check that top-level stuff exists as expected
#     - Maybe this should be left to the officer error handling, which checks
#       master and layout existence anyway...
#   - For a given slide, check that all placeholders exist
#     - If not: error, and display officer::plot_layout_properties so the user
#       can visually spot what's wrong
#   - Enforce UUIDs
#   - Enforce data dumping
#
# ---
# 
# Yooo what if we get silly and just create aliases for list()?
# 
# powerpoint(
#   slide(
#     master = ...,
#     layout = ...,
#     plot = ...,
#     title = ...
#   )
# )
# 
# No, actually, you know what? slide() and powerpoint() should both be the 
# functions that do things. That also means that the documentation/help can be 
# more accessible (both in terms of literally pulling up what's going on
# 'further down', and how comprehensible it is)

# Requires =====================================================================

box::use(
  assertthat[assert_that],
  glue[glue]
)

# Module: Data dumping =========================================================

ensure_dir_exists <- function(file_out){
  
  target_dir <- dirname(file_out)
  
  dir.create(target_dir,
             showWarnings = TRUE,
             recursive = TRUE)
  
  assert_that(dir.exists(target_dir), 
              msg = glue("Couldn't create {target_dir}"))
  
  return(invisible(TRUE))
  
}


dump_artefact_data <- function(x, 
                               file_out, 
                               recursive = FALSE, 
                               ...){
  
  UseMethod("dump_artefact_data")
  
}


dump_artefact_data.ggplot <- function(x, 
                                      file_out, 
                                      recursive = FALSE, 
                                      ...){
  
  ensure_dir_exists(file_out)
  
  x$data |> write_csv(file_out)
  
  return(invisible(TRUE))
  
}


dump_artefact_data.viz_artefact <- function(x,
                                            file_out,
                                            recursive = FALSE,
                                            ...){
  
  ensure_dir_exists(file_out)
  
  x$data |> write_csv(file_out)
  
  return(invisible(TRUE))
  
}


# Module: UUIDs for tracking ===================================================

#' Add chart UUID somewhere safe: to slide notes!
#'
#' @param ppt officer ppt object
#' @param uuid uuid of viz artefact
#'
#' @description We add our UUIDs to the notes as this should be safe for like
#'   >99% of cases. We *could* add it to the 'speaker notes', which translates
#'   to the 'body' placeholder on the Notes View / default Notes Master.
#'   However, this is still visible to the layperson and might create
#'   confusion... so we'll add it where no admin person will look: Notes View!
#'   \(Specifically we'll add it to the notes footer, as that was the most
#'   aesthetically pleasing spot, excluding the body.\)
#'
#'   There were other potential solutions in this space - such as adding a small
#'   textbox with background-colored text with the UUID; see also \{ggtrack\}.
#'   However, I was too paranoid for this: when people inevitably tweak things
#'   directly in Powerpoint, this could get lost too easily for my taste.
#'
#'   You may also ask - why bother with this at all, when we're also changing
#'   our workflow for compiling Powerpoints? Why not just trace backwards from
#'   output, to much-more-clearly-articulated compilation, to the source for an
#'   artefact in a given slide list/chunk/thing?
#'
#'   Well - I think the UUID should still make it faster: if you output a fairly
#'   bare slide, and the text gets heavily re-written, and the slide gets
#'   repositioned in the deck, then it would still be a little slower to match
#'   up code-slide with final-slide than it would be to jump straight to the
#'   single instance of a hash in the codebase.
#'
#' @return officer ppt object with annotations on the current slide's notes
#' @export
#'
#' @examples
add_uuid_to_speaker_notes <- function(ppt,
                                      uuid){
  
  # TODO: build support for multiple objects
  
  # Hmmm, now I'm second-guessing this; I feel like this becomes less necessary
  # if we're refactoring how powerpoints get compiled to begin with?

  ppt |> 
    
    officer::set_notes(
      glue(
        "Chart UUIDs = {uuid}"
      ),
      location = officer::notes_location_type("ftr")
    )
  
}

# See also snippet "plot_skeleton" for synergies.


# Module: validation of template? ==============================================

validate_template_compatibility <- function(path_to_ppt){
  
  # Read in ppt
  
  # Heuristics: do any layouts have, like, nice placeholder names?
  #   If not, emit warning
  #   (Maybe just once per session?)
  
  # Enforce: notes master...?
  #   Probably too much - let's just use the standard notes master format
  
}


# Module: individual slides ====================================================


#' Title
#'
#' @param ppt 
#' @param ... 
#' @param uuid 
#' @param layout 
#' @param master 
#'
#' @return
#' @export
#'
#' @examples
create_arbitrary_slide <- function(ppt,
                                   ..., # ~Flexible~
                                   uuid = NULL,
                                   layout,
                                   master){
  
  # TODO: once design pattern per Thinking above is decided, refactor args
  
  layout <- match.arg(layout)
  
  assert_that(
    !is.null(uuid),
    msg = "Please provide a unique identifier for the chart. This makes it easier to trace outputs back to code and data."
  )
  
  
  objects_for_slide <- list(...)
  
  objects_for_slide_names <- names(objects_for_slide)
  
  assert_that(
    !any(is.na(objects_for_slide_names) | is.null(objects_for_slide_names) | objects_for_slide_names == ""),
    msg = "Looks like something has been passed to ... that doesn't have a name. Check dots are all named, and that later arguments haven't been cannibalised..."
  )
  
  # TODO: check validity of objects specified
  #   Do they exist in the layout provided
  
  # TODO: push this upstream - tweak the .pptx "template".
  
  # dict_layout <- yaml::read_yaml("slide_layout_mapping.yaml") |> 
  #   
  #   purrr::pluck(layout)
  
  # TODO:
  #   - iterate over objects_for_slide
  #   - if there's a method for its class, export its data
  #     - if there are multiple objects (e.g. >1 chart to a slide) then name
  #       according to its placeholder reference
  #   - 
  
  ppt |> 
    
    add_slide(layout, master) |> 
    
    reduce(
      
      .init = _,
      
      .x = plot_objects,
      
      .f = ...
        
    )
  
  
  # Code to plug in:
  # ph_with(
  #   
  #   # If we think the 'plot' obj is a ggplot, attempt to convert to vector
  #   # graphic;
  #   value = if("ggplot" %in% class(plot_obj)){
  #     dml(code = print(plot_obj))
  #   } else {
  #     # Otherwise (e.g. it's actually a flextable), attempt to pass directly
  #     plot_obj
  #   }, 
  #   
  #   location = ph_location_label("Content Placeholder 2")
  #   
  # ) %>%
  #   
  #   ph_with(
  #     value = glue(title), 
  #     location = ph_location_label("Title 21")
  #   ) %>%
  #   
  #   ph_with(
  #     value = glue(footer), 
  #     location = ph_location_label("Footer Placeholder 3")
  #   )
  # 
  
  
}


# Module: rendering ============================================================

#' Title
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
render_powerpoint <- function(...){

    
  
}
