
# General-purpose ==============================================================

library(parallel)
library(doSNOW)


logger::log_info("Calculating skill similarities. If you do not see a progress bar shortly, something has gone wrong.")

# This approach gives us more fine-tuned control, which can be useful when e.g.
# using weird connection methods for databases

## ===== Parallelisation =====================================

n_cores <- detectCores(logical = TRUE)

# Spin up a cluster and sink console output to a txt log for inspection
parallel_cluster <- makeCluster(
  min(c(4, n_cores-2)), # This approach is generally not recommended - and certainly not *within* packages.
  outfile = "parallel-logs.txt"
)

# Make sure it gets spun down when the function ends, no matter what...
on.exit(stopCluster(parallel_cluster))

# Make sure we register that cluster
registerDoSNOW(parallel_cluster)

# Initialisation:
clusterEvalQ(parallel_cluster, {
  
  library(DBI)
  library(glue)
  library(lubridate)
  library(openxlsx)
  library(tidyverse)
  
  # Need the actual workhorse functions
  source("R/your-other-functions.R")
  source("R/connect-to-DB.R")
  
  # clusterEvalQ returns whatever the expression does, per node, so just
  # throw this in here
  NULL
  
  # So you can be thoughtful about whether you e.g. have a package that sets up
  # a default DB connection method, and then a script that overrides or curries
  # that and so on
  
})

# You could also do funky stuff with using keyring or passing around ephemeral
# credentials (or, I mean, there's always text files on disk...)

# if(X){
# 
#   clusterExport(parallel_cluster, c("db_uid", "db_pwd"))
#   
#   clusterEvalQ(parallel_cluster, {
#     db_pwd
#     db_uid
#     source("R/override-connect-to-DB.R")
#   })
#   
#   
# }

# Or manage that in the main thread and just pass one method through.
clusterExport(parallel_cluster, "my_db_conn_method")

# Now with the right connection method in place, fire!
clusterEvalQ(parallel_cluster, {conn <- my_db_conn_method(); NULL})
# The NULL here is more important that the one above, as returning a conn object
# breaks things because it can't be serialized for transmission back to the
# manager session...


## ===== The doing =====================================

iterand <- c(1:1000)

logger::log_info(glue("Total queries to be run: {iterand}"))


# One tick per top-level iterand
my_progress <- progress::progress_bar$new(total = length(iterand))

# This function is so doSNOW can handle progress in the parallel section
my_progress_fn <- function(n){my_progress$tick()}

# Make it show before parallel functionality
my_progress$tick(0)


final_result <- foreach(
  
  iterand_i = iterand, 
  
  # Note the .noexport option! foreach automagically exports required objects
  # to the clusters, and we DON'T want to try to serialise the manager 
  # session's DB connection (hint: that doesn't work)
  .noexport = c("conn"),
  
  # Provide progress via doSNOW's handling - takes a function to tick an 
  # existing progress bar
  .options.snow = list(progress = my_progress_fn),
  
  # So the final result will be the list of similarity tibbles, auto-combined
  .combine = bind_rows
  
) %dopar% {
  
  # Then anything that's specified here and NOT in .noexport will get figured
  # out and dispatched accordingly. Not sure if that does static analysis or
  # what.
  
  ...
    
}


# In {targets} =================================================================

# Way easier.

# Top of _targets.R:

tar_option_set(
  
  # ~~~ Parallel processing ~~~
  
  deployment = "main", # Default to main process (~"thread", but not really)
  controller = crew::crew_controller_local(
    name = "parallel_controller",
    workers = 6
  )
  
)

# In pipeline definition:

tar_target(my_iterand, c(1:1000)) # Will run on main thread
tar_target(my_iterand_2, c(1:2))

tar_target(
  
  my_function(
    
    # Specify targets in fn arguments
    key_iterand = my_iterand,
    other_iterand = my_iterand_2,
    
    ...
    
  ),
  
  # Specify how we're going to use that target / those targets:
  pattern = map(cross(my_iterand, my_iterand_2)),
  
  deployment = "worker" # Parallelise this
  
)
