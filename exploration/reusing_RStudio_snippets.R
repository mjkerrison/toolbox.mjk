
# install.packages("renv")

# Latest version was 0.0.0.9, 9 years ago (Apr 15, 2015)
renv::install("dgrtwo/snippr@e26cd436cd99e49db095a0c2ca456a13a5e03aa5")

# It relies on a function (devtools::github_pat()) that was removed several
# versions of {devtools} ago...
snippr::snippets_install_github("mjkerrison/toolbox.mjk",
                                language = "devops",
                                name = "install_from_ADO")

# But the _add and _remove functions are nice; maybe we can install from URL?

repo_stub <- "https://github.com/mjkerrison/toolbox.mjk/blob/dev/snippets"

snippr::snippets_install_url(
  glue::glue("{repo_stub}/devops.snippets"),
  name = "install_from_ADO",
  language = "r"
)

# Nope - taking a look at that...
# req <- httr::GET(glue::glue("{repo_stub}/devops.snippets"))
# txt <- httr::content(req, as = "text")
# snippr:::snippets_parse(txt)
# ...there's clearly more that's changed in the last 9 years.

# So we'll need to fork or re-write our own.
