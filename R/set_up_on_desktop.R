
# The need for this would be obviated if using something online like Posit
# Workbench where this can be managed centrally.


#' Get set up for R at X
#'
#' @description
#'
#' Principles:
#'  - This should be able to be run multiple times in succession without causing
#'    problems - e.g., avoid writing the same lines to .Rprofile multiple times.
#'    This way, when people inevitably have multiple issues in a row, we can
#'    just run this over and over as we go.
#'
#' NOT checked for:
#'  - RTools: should only be required by advanced users. We've had trouble with
#'    this in the past, and this setup now sets the default repository to one
#'    with many more binaries, so we won't worry about this for the majority of
#'    R installers.
#'  - Git: we could probably check this with \{git2r\} but this hasn't generally
#'    been an issue historically, so we're leaving it aside for now.
#'
#'
#' @return
#' @export
#'
#' @examples
get_set_up_X <- function(.target_R_version){
  
  # 1) Verify general installation features ====================================
  
  message("⏳ Checking R version...")
  
  actual_R_version <- paste0(version$major, ".", version$minor)
  
  if(actual_R_version < .target_R_version){
    stop(
      paste0("❌ Expected at least R ", .target_R_version, " but ", actual_R_version, "is installed - please ask for help")
    )
  }
  
  message("✅ R version OK!")
  
  # 2) Change package installation defaults ====================================
  
  message("⏳ Changing package installation settings...")
  
  # This is a string broken into lines so that it writes to .Rprofile with the
  # right formatting (basically just the indentation, and just aesthetic).
  package_install_options <- c(
    "options(",
    "  pkgType = 'binary',",
    "  repos = c(",
    "    'P3M' = 'https://packagemanager.posit.co/cran/latest',",
    "    'CRAN' = 'https://cloud.r-project.org'",
    "  )",
    ")"
  )
  
  ## --- For the session -------------------------------------------------------
  # So we don't need the user to restart R for this to take effect
  
  # Just source that .Rprofile code as-is!
  source(textConnection(package_install_options))
  
  ## --- In .Rprofile ----------------------------------------------------------
  # So it persists across sessions (for most users - other than {renv}, most
  # projects don't have a project-level .Rprofile.
  
  # Check if this is already in .Rprofile:
  current_rprofile <- readLines(file.path("~", ".Rprofile"))
  
  is_in_current_rprofile <- grepl(toString(package_install_options),
                                  toString(current_rprofile),
                                  fixed = TRUE)
  
  if(is_in_current_rprofile){
    
    message("⏩ Package installation settings already OK.")
    
  } else {
    
    write(package_options, file = file.path("~", ".Rprofile"), append = TRUE)
    
    message("✅ Updated package installation settings!")
    
  }
  
  # 3) Install {renv} to install other things ==================================
  
  message("⏳ Installing package {renv} for further package installation...")
  
  if(length(find.package("renv", quiet = TRUE)) > 0){
    
    message("⏩ Skipping ({renv} already installed)...")
    
  } else {
    
    message("-----------------------------------")
    
    install.packages("renv")
    
    message("-----------------------------------")
    message("✅ Installed {renv} successfully!")
    
  }
  
  # 4) Install most public packages ============================================
  # We do this in one go to minimise how many times we go through the {renv}
  # process, which isn't very beginner-friendly console output...
  
  message("⏳ Installing key public packages...")
  message("-----------------------------------")
  
  target_packages <- c(
    
    # Core packages
    "tidyverse",
    "ggrepel",
    "R.utils",
    "janitor",
    "odbc",
    "stringdist",
    "shiny",
    "officer",
    "Cairo",
    "flextable",
    "miniUI",
    "rvg",
    "svglite",
    "pryr",
    "remotes",
    "sf",
    "lwgeom",
    "glue",
    
    # Dependencies of other (esp. internal) packages
    # (Some may need particular versions, which {renv} makes easy to express)
    "Rttf2pt1", # "Rttf2pt1@1.3.8"
    "extrafont",
    "extrafontdb", # "extrafontdb@1.0",
    
    # For further installation steps
    "rstudioapi",
    "crayon"
    
  )
  
  renv::install(target_packages, prompt = FALSE, verbose = FALSE)
  
  message("-----------------------------------")
  message("✅ Installed key public packages!")
  
  # 5) In-house packages =======================================================
  
  print_X <- crayon::style(
    # TODO drop in here
  )
  
  message(paste0("⏳ Installing ", print_X, " packages..."))
  message("👀 You may get a pop-up for Git credentials. Open in browser to sign in with SSO!")
  message("-----------------------------------")
  
  # Install directly from DevOps
  renv::install(
    
    # TODO drop in here
    
    prompt = FALSE,
    verbose = FALSE
    
  )
  
  message("-----------------------------------")
  message(paste0("✅ Installed ", print_X, " packages!"))
  
  # 6) Community packages ======================================================
  
  message("⏳ Installing community packages...")
  message("👀 If you see a warning about GITHUB_PAT, you can safely ignore it.")
  message("-----------------------------------")
  
  renv::install(
    
    c("wfmackey/absmapsdata",
      "MattCowgill/readabs",
      "runapp-aus/strayr"),
    
    prompt = FALSE,
    verbose = FALSE
    
  )
  
  message("-----------------------------------")
  message("✅ Installed community packages!")
  
  # 7) Set RStudio global settings ---------------------------------------------
  
  message("⏳ Tweaking RStudio settings...")
  
  # Documentation for valid options and values can be found at:
  # https://docs.posit.co/ide/server-pro/reference/session_user_settings.html
  
  # IMPORTANT! For reproducibility:
  # rstudioapi::writeRStudioPreference("save_workspace", "never")
  # rstudioapi::writeRStudioPreference("load_workspace", FALSE)
  # 
  # # For more style / formatting consistency across users:
  # rstudioapi::writeRStudioPreference("insert_native_pipe_operator", TRUE)
  # rstudioapi::writeRStudioPreference("strip_trailing_whitespace", TRUE)
  # rstudioapi::writeRStudioPreference("auto_append_newline", TRUE)
  # 
  # # For UX:
  # rstudioapi::writeRStudioPreference("scroll_past_end_of_document", TRUE)
  # rstudioapi::writeRStudioPreference("reduced_motion", TRUE)
  # rstudioapi::writeRStudioPreference("git_diff_ignore_whitespace", TRUE)
  
  # TODO: convert to {usethis} for nicety
  usethis::use_rstudio_preferences(
    "save_workspace" = "never",
    "load_workspace" = FALSE,
    "graphics_backend" = "ragg" # This one is a doozy...^1
  )
  
  # 1. 
  # https://ragg.r-lib.org/
  # https://github.com/r-lib/systemfonts
  # https://www.cararthompson.com/posts/2024-01-12-using-fonts-in-r-for-dataviz/2024-01-12_getting-fonts-to-work
  # Graphics devices are complicated...
                                   
  
  # TODO: look into the "panes" preference???
  #   One could probably use something similar to this to replicate *one's own*
  #   preferred setup...
  
  message("✅ Tweaked RStudio settings!")
  
  # Done! ======================================================================
  
  message("🏆 Ran install script from start to end!")
  
  return(invisible(NULL))
  
}


#' Verify X setup
#'
#' @return
#' @export
#'
#' @description
#' Runs a query and does some visualisation that should \(only\) work if
#' everything's installed correctly.
#'
#'
#' @examples
verify_X_setup <- function(){
  
  # This has been re-written to minimise impacts on the global environment.
  
  message("🔍 Testing setup...")
  
  # Get verifying --------------------------------------------------------------
  
  # ...
  
  return(invisible(NULL))
  
}

# Run! =========================================================================

get_set_up_X()

verify_X_setup()
