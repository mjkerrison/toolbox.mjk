
library(memoise)
library(tidyverse)

test_fn <- function(...){
  
  return_val <- 42
  
  Sys.sleep(5)
  
  return(return_val)
  
}

test_fn <- memoise(test_fn,
                   cache = cachem::cache_disk(dir = "temp"))


test_fn(5)

test_fn(5)

# And now testing reboot
# Ah-hah - so the caching *is* consistent across sessions...
# But changing it does invalidate it!
# Excellent...
