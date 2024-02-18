# This script updates the default list of files (everything in R/) that
# populates ~/use_toolbox/use_toolbox.R - called from a GitHub Action on any
# push to main.

# TODO: convert this to CLI in the Actions .yml?

all_r_files <- list.files("R/", "\\.R$", full.names = FALSE)

use_toolbox_content <- readLines("use_toolbox/use_toolbox.R")

last_static_line <- grep("^download_from_github\\(c\\($", use_toolbox_content)

new_use_toolbox_content <- c(
  
  use_toolbox_content[1:last_static_line],
  
  paste0("  'R/", all_r_files, "',"),
  
  "))",
  
  ""
  
)

writeLines(new_use_toolbox_content, con = "use_toolbox/use_toolbox.R")
