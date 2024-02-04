
# - Moved away from bringing things into global

# - Moved away from having a reset() function to clean up these utilities - now
#   just using Ctrl+Shift+F10 to restart the R session!

# - Have moved to using package "import" to bring things in from arbitrary
#   scripts - handles all the sourcing and putting things in the right 
#   environments - no longer need to stash things in a list and attach() that


check_productivity <- function(){
  # Built around flat project structures lol - need to rethink this these days
  dir() %>%
    {grep("\\.R$", ., value = T)} %>%
    map(read_lines) %>%
    map_dbl(length) %>%
    sum()
}




is_dttm <- function(tibble_col){

  all(class(tibble_col) == c("POSIXct", "POSIXt"))

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Thanks https://stackoverflow.com/questions/37610056
`%!=na%` <- function(e1, e2) (
  e1 != e2 |
    (is.na(e1) & !is.na(e2)) |
    (is.na(e2) & !is.na(e1))
) & !(is.na(e1) & is.na(e2))

`%==na%` <- function(e1, e2){!`%!=na%`(e1, e2)}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

write2clip <- function(input_tbl, na_overwrite = "NA",
                       headers = T, clip_mem = 2^15,
                       ...){

  input_tbl %>% write.table(file = paste0("clipboard-", clip_mem),
                            sep = "\t",
                            row.names = F,
                            na = na_overwrite,
                            col.names = headers,
                            ...)

}


read_clippy <- function(header_or_not = T,
                        read_fn = c("table", "delim")){

  # Get this:
  # https://kbroman.org/blog/2017/08/08/eof-within-quoted-string/
  # read.table and read.delim have different default arguments around quotes,
  # which tend to be important.
  # We /could/ look into that and think and do something better than switching
  # functions, OR we could do the quick n dirty thing and just let me pick
  # at run-time...

  read_fn <- get(paste0("read.", match.arg(read_fn)))

  return(
    as_tibble(
      read_fn(
        "clipboard",
        sep = "\t",
        header = header_or_not
      )
    )
  )

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This one a small utility to get column # from excel reference (Z, AA, AB...)
# Thanks Edo @ https://stackoverflow.com/questions/34537243
letters2numbers <- function(x){

  # letters encoding
  encoding <- setNames(seq_along(LETTERS), LETTERS)

  # uppercase
  x <- toupper(x)

  # convert string to a list of vectors of single letters
  x <- strsplit(x, split = "")

  # convert each letter to the corresponding number
  # calculate the column number
  # return a numeric vector
  sapply(x, function(xs) sum(encoding[xs] * 26^((length(xs)-1):0)))

}


numbers2letters <- function(x){

  digits <- c()

  while(x){

    y <- ifelse(x %% 26 == 0, 26, x %% 26)

    digits <- c(digits, LETTERS[y])

    x <- (x - y) / 26

  }

  return(paste(rev(digits), collapse = ""))

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

tibblify_list <- function(to_tibblify){

  longest <- to_tibblify %>% map_dbl(length) %>% max(na.rm = T)

  map_dfc(to_tibblify, function(x){

    c(x, rep(NA_real_, times = longest - length(x)))

  }) %>% return()

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_EOFY <- function(date_in){

  lubridate::dmy(
    paste0(
      "30/6/",
      floor(lubridate::quarter(date_in, with_year = T, fiscal_start = 7))
    )
  )

}


get_SOFY <- function(date_in){

  lubridate::dmy(
    paste0(
      "1/7/",
      floor(lubridate::quarter(date_in, with_year = T, fiscal_start = 7)) - 1
    )
  )

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Small wrapper to just read in _all_ sheets from an Excel workbook

read_excel_sheets <- function(workbook_path){

  all_sheets <- readxl::excel_sheets(workbook_path)

  return(setNames(

    map(all_sheets, function(sheet_i){
      readxl::read_excel(path = workbook_path, sheet = sheet_i)
    }),

    all_sheets

  ))

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

num2hex <- function(input_numeric){

  return(tibble::tibble(

    hexified = toupper(as.character(as.hexmode(input_numeric))),
    nchar_zeroes = 16 - nchar(hexified),
    zeroes = purrr::map_chr(nchar_zeroes, ~paste(rep("0", .), collapse = "")),
    result = paste0("0x", zeroes, hexified)

  )$result)

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

distinctiveness <- function(tbl_to_check, cols_to_group_by){

  tbl_to_check %>%
    group_by(!!!syms(cols_to_group_by)) %>%
    summarise(Count = n()) %>%
    dplyr::arrange(desc(Count)) %>%
    return()

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

unfill <- function(x){

  # This one courtesy of https://github.com/tidyverse/tidyr/issues/250
  # From the big man himself - because "it's never getting added" lol

  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, NA, x)

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Thanks Richie Cotton: https://stackoverflow.com/questions/7145826/
percent <- function(x, digits = 2, format = "f", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}




rowwise_mutate <- function(tbl_in, ...){
  return(ungroup(mutate(rowwise(tbl_in), ...)))
}

# Meta Management --------------------------------------------------------------

# Don't really need to manage anything about MK_Utilities from here because
# we'll just import::from it instead - so if we want to look at anything or 
# edit it we can just open it in the monorepo project instead.


# ... Listing projects -------------------------------------

# I saw some stuff online about e.g. how Hadley Wickham uses some Mac app to
# surface all his projects - well, we can do a similar thing in Windows just by
# searching for "*.Rproj" in the top-level project directories - and then you
# can actually save the search like a shortcut that you can then open in
# Explorer, pin to Start etc. etc. (easiest: click and drag stack-of-blue-files 
# icon in the path bar to the folder)


# ... Profile/Environ --------------------------------------

# https://stackoverflow.com/questions/46819684/
# 1) R_HOME: the directory in which R is installed. Find out where your
# R_HOME is with the R.home() command.
# 2) HOME, the user’s home directory. Can ask R where this is with,
# path.expand("~")
# 3) R’s current working directory. This is reported by getwd().

# I never remember this lol


edit_Rprofile <- function(){
  file.edit(paste0(path.expand("~"), "/.Rprofile"))
}


# ... Renviron ---------------------------------------------

# This all being the case, though, we WILL need to add a couple things to
# the .Renviron:

edit_Renviron <- function(){
  file.edit(paste0(path.expand("~"), "/.Renviron"))
}

# monorepo="Path:/To/Monorepo"
# MK_Utilities="Path:/To/Monorepo/MK_Utilities.R"
# datalake="Path:/To/Local Data Lake"


# ... Importing from the monorepo --------------------------

import <- function(x, repo = Sys.getenv("monorepo"), ...){
  
  import::from(paste0(repo, "/", x), ...)
  
}


# ... Self-containing --------------------------------------

# Need to write something up that pulls dependencies (code, data?) into the
# local project directory and then redirects code to point at that?
# ...And zips it up? Which should probably be added to .gitignore?

# We can probably leverage .Renviron again for that - we could use this to
# generate a _project-local_ .Renviron that overrides the user's .Renviron - no
# change to code necessary (?) as it should all just point at (the newly-
# generated) "/src" folder (or similar)


# ... Unsolved ---------------------------------------------

# Final problem: maintaining info about what version of the monorepo stuff
# we're using
#   - Could just write in the git commit hash at the top of a project script?



# RSQLite utilities --------------------------------------------

# So much for writing functions to work without the dependencies loaded LOL
# That's now well out the window, and further up the script as well
# We're now using a lot more "real" SQL, so we'll probably pivot harder to
# using DBI to connect and dplyr to manipulate - which _also_ work for local
# SQLite databases as well.


# Version of the 'write' function that turn (lubri)dates into characters

# MK_dbAppendTable <- function(tbl_to_append,
#                              connection,
#                              db_tbl_name){
# 
#   tbl_to_append %>%
#     mutate(across(where(is.Date), as.character)) %>%
# 
#     {RSQLite::dbAppendTable(
#       conn = connection,
#       name = db_tbl_name,
#       value = .
#     )}
# 
#   return(TRUE)
# 
# }

# Version of the 'read' function that turn specified columns back into
# dates - unfortunately no other 100% reliable way

# MK_dbGetQuery <- function(query_string,
#                           connection,
#                           cols_to_dateify = NULL,
#                           date_fn = lubridate::ymd,
#                           ...){
# 
#   RSQLite::dbGetQuery(conn = connection,
#                       statement = query_string,
#                       ...) %>%
# 
#     dplyr::as_tibble() %>%
# 
#     mutate(across(cols_to_dateify, date_fn)) %>%
# 
#     return()
# 
# }


# MK_dbDisconnect <- function(conn_obj){
# 
#   # Slightly annoying that we'll be passing in a string here when we pass in
#   # the actual connection object to everything else, but this seems like the
#   # most elegant way...
# 
#   # OH - new approach, though still hacky: take the conn object, but use it
#   # as a quote and de-quote-slash-sym-ify it
#   conn_name <- enexpr(conn_obj) %>% rlang::as_string()
#   # Lmao: note the use of enexpr, NOT quote - enexpr is lazy, while quote
#   # was eager (more or less? https://adv-r.hadley.nz/quasiquotation.html)
#   # so quote() was yielding 'conn_obj' PER SE, while enexpr correctly gives
#   # the object that was passed in.
# 
# 
#   if(conn_name %in% ls(globalenv())){
# 
#     RSQLite::dbDisconnect(conn_obj)
# 
#     rm(list = c(conn_name), pos = globalenv())
# 
#     return(TRUE)
# 
#   } else {
# 
#     print("Connection not found!")
#     return(FALSE)
# 
#   }
# 
# }



# MK_dbConnect <- function(filepath){
# 
#   return(dbConnect(RSQLite::SQLite(), filepath))
# 
# }




# Functions with prescribed inputs ---------------------------------------------

# Note that this approach will pick the first result if nothing is specified -
# which may or may not be what you want...

# my_function <- function(x = c("a", "b")){
#
#   x <- match.arg(x)
#
#   print(x)
#
# }


# Excel date format ------------------------------------------------------------

# https://stackoverflow.com/questions/43230470
# Key fact: Excel date origin is 30/12/1899. Thanks Andrew Breza.
# Refer originXL() above.

originXL <- function(){lubridate::dmy("30/12/1899")}


# Encoding ---------------------------------------------------------------------

# I have a doozy of a time getting some reports generated by some systems to
# stop showing en dashes as some permutation of <U+0096>...
# Looks like it's being pooped out with encoding windows-1252, so doing
# something like:

# read_tsv(
#   path,
#   locale = locale(encoding = "windows-1252"),
#   skip = 5, guess_max = 1000000
# )

# Gets it in right. Weirdly, readxl::read_excel is really good at this with xl
# files - I guess it's something more explicit or concrete courtesy of Excel,
# idk.

# Thank you so much to Gladys_C_Hugh:
# https://stackoverflow.com/questions/36108790


# R date formatting ------------------------------------------------------------

# https://www.r-bloggers.com/2013/08/date-formats-in-r/

# Conversion specification	Description	Example
# %a	      Abbreviated weekday                   Sun, Thu
# %A	      Full weekday                       Sunday, Thursday
# %b or %h	Abbreviated month	                    May, Jul
# %B	      Full month                            May, July
# %d	      Day of the month (01-31)               27, 07
# %j	      Day of the year 001-366	              148, 188
# %m	      Month 01-12	                           05, 07
# %U	      Week 01-53 w/ Sun as 1st of the week   22, 27
# %w	      Weekday 0-6 Sunday is 0	                0, 4
# %W	      Week 00-53 w/ Mon as 1st of the week   21, 27
# %x	      Date, locale-specific
# %y	      Year without century 00-99             84, 05
# %Y	      Year w/ century on input:            1984, 2005
#             00-68 prefixed by 20
#             69-99 prefixed by 19
# %C	      Century                                19, 20
# %D	      Date like %m/%d/%y               05/27/84, 07/07/05
# %u	      Weekday 1-7; Monday is 1                7, 4
#
# %n	      Newline on output or arbitrary whitespace on input
# %t	      Tab on output or arbitrary whitespace on input


# Managing Projects in Windows -------------------------------------------------

# I saw some stuff online about e.g. how Hadley Wickham uses some Mac app to
# surface all his projects - well, we can do a similar thing in Windows just by
# searching for "*.Rproj" in the top-level project directories - and then you
# can actually save the search like a shortcut that you can then open in
# Explorer, pin to Start etc. etc. (easiest: click and drag stack-of-blue-files 
# icon in the path bar to the folder)


# My Snippets ------------------------------------------------------------------

# - Put these in an "r.snippets" file in the monorepo
# - Small function to put those in the RStudio user profile at:
#     %appdata%/Roaming/RStudio/snippets/

# Other snippet pro-tips:
#   - Use Shift+Tab to auto use the snippet rather than Tab->Tab(->Tab...) to
#     get it from the autofill suggestions

push_snippets <- function(){
  
  snippets_pushed <- paste0(Sys.getenv("monorepo"), "/r.snippets")
  
  snippets_target <- paste0("C:/Users/", 
                            Sys.getenv("USERNAME"), 
                            "/AppData/Roaming/RStudio/snippets/r.snippets")
  
  if(file.exists(snippets_pushed)){
    
    file.copy(snippets_pushed, snippets_target, overwrite = T)
   
    return(T)
     
  } else return(F)
  
}


