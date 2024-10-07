
library(memoise)
library(tidyverse)
library(glue)

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


test_fn_2 <- function(...){
  Sys.sleep(4)
  return(68)
}

test_fn_3 <- function(...){
  Sys.sleep(4)
  return(70)
}



all_functions <- readLines("exploration/exploring_memoisation.R") |> 
  
  purrr::keep(
    \(x) stringr::str_detect(x, " <- function\\(")
  ) |> 
  
  stringr::str_extract(".+(?=<-)") |> 
  
  stringr::str_replace("\\s", "")



for (fn_name in all_functions){
  
  fn_object <- get(fn_name)
  
  if(!memoise::is.memoised(get(fn_name))){
    
    print(glue("{fn_name} is not memoised"))
    
    assign(
      fn_name,
      value = memoise::memoise(
        fn_object,
        cache = cachem::cache_disk(dir = "temp")
      )
    )
    
  } else {
    
    print(glue("{fn_name} is already memoised"))
    
  }
  
}


test_fn_2(5)

test_fn_3(5)

# Excellent - and this works with automating the memoisation!
