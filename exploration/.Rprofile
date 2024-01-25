# Setting up somewhere new -----------------------------------------------------

# - Don't forget to adjust global settings about .RData etc.
# - Don't forget to check "Ensure source files end with newline"

# Loading RProfile -------------------------------------------------------------

# So here's the plan: let's Github this, and then just set R to use it straight
# out of the local repo using CMD:
# SETX R_PROFILE_USER "C:/path/to/repo/.Rprofile"
# Thanks https://stackoverflow.com/questions/15225990/how-to-change-rprofile-location-in-rstudio)


# Loading packages -------------------------------------------------------------

# https://stackoverflow.com/questions/25997618/loading-dplyr-via-rprofile-site-in-rstudio :

# In the startup order for R (see ?Startup), .First() is called before
# .First.sys() which loads the other packages. It is the stats package that
# overwrites filter.
#
# .First.sys() uses options("defaultPackages") to determine what to load, so I
# suggest you edit that in your .First() function with:

.First <- function(){
  
  options(
    
    defaultPackages = c(
      
      # Keep the normal stuff - don't wanna mess with that
      getOption("defaultPackages"),
      
      # Absolutely core
      "tidyverse",
      
      # I want the rest of the pipes - sorry Hadley
      "magrittr",
      
      # So useful for dates n types; tidyverse, but not default
      "lubridate",
      
      # My go-to for plotting
      "plotly"
      
    )
    
  )
  
}


# Custom utilities -------------------------------------------------------------

# Apparently objects prefixed with a period do not show up in ls(), and thus
# also do not get yeeted by rm(list = ls()).
# So what we can do (https://csgillespie.github.io/efficientR/set-up.html) is
# set up a 'hidden' environment and stash custom stuff in there, then attach()
# it and hey presto

.env <- new.env()

# Thank you http://jdobr.es/blog/compound-inequalities-r/


.env$compound_inequality <- function(lhs, rhs, comparison) {
  if (is.null(attr(lhs, 'compound-inequality-partial'))) {
    out <- rhs
    attr(out, 'compound-inequality-partial') <- do.call(comparison, list(lhs, rhs))
  } else {
    out <- do.call(comparison, list(lhs, rhs)) & attr(lhs, 'compound-inequality-partial')
  }
  
  return(out)
}

.env$'%<<%' <- function(lhs, rhs) {
  return(compound_inequality(lhs, rhs, '<'))
}

.env$'%<=%' <- function(lhs, rhs) {
  return(compound_inequality(lhs, rhs, '<='))
}

.env$'%>>%' <- function(lhs, rhs) {
  return(compound_inequality(lhs, rhs, '>'))
}

.env$'%>=%' <- function(lhs, rhs) {
  return(compound_inequality(lhs, rhs, '>='))
}

attach(.env)

# Other bits -------------------------------------------------------------------

# Great idea that everyone uses apparently
options(stringsAsFactors = F)

# And a lil confirmation that we've loaded this stuff
# For some reason when opening other projects the normal console output (info 
# about R and tidyverse) doesn't appear?
#.Last <- function(){print("Loaded MK's .RProfile successfully.")}
message("Loaded MK's .RProfile successfully.")
