
# Using {renv} =================================================================

# From https://rstudio.github.io/renv/articles/renv.html#libraries-and-repositories:

# You can see which repositories are currently set up in your session with
# getOption("repos"); when you call install.packages("{pkgname}"), R will look
# for pkgname in each repository in turn.

# So then we'll have a project-local .Rprofile file that looks like:

#   options(pkgType = "win.binary")
#   options(repos = c(PPPM = "https://packagemanager.posit.co/cran/latest", getOption("repos")))
#   source("renv/activate.R")

# To force installation from binaries, and to prefer Posit Public Package
# Manager (which seems to have a much more comprehensive set of {R version,
# Package version} combinations pre-built than CRAN itself does...)

# So then?

# renv::snapshot() - and let it handle it from there.

# But with some manual hand-holding when it comes to GitHub-only packages:

# renv::install("dgrtwo/snippr@e26cd436cd99e49db095a0c2ca456a13a5e03aa5")
