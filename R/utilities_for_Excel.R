

#' Memo to self: Excel's date origin
#'
#' @description Via https://stackoverflow.com/questions/43230470 - thank you
#'   Andrew Breza. Occasionally when you read in dates from Excel something goes
#'   funky \(number formatting IIRC\) and you need to convert to date manually.
#'   This means you need to know the fact that Excel's date origin is
#'   30/12/1899. That's all this is for.
#'
#' @return
#' @export
#' 
#' @importFrom lubridate dmy
#'
#' @examples
originXL <- function(){
  lubridate::dmy("30/12/1899")
}




#' Convert an Excel column reference to a number \(e.g. A => 1\)
#'
#' @description A small utility to get a column number from an Excel reference.
#'   This is most salient for very wide workbooks where you end up with like ZG
#'   - what number is that?? Taken from Edo at 
#'   https://stackoverflow.com/questions/34537243.
#'
#' @param x A vector of numbers
#'
#' @return
#' @export
#'
#' @examples
XL_letters2numbers <- function(x){
  
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



#' Convert a number to an Excel column reference \(e.g. 1 => A\)
#'
#' @description Inverts `XL_letters2numbers`.
#'
#' @param x A vector of numbers
#'
#' @return
#' @export
#'
#' @examples
XL_numbers2letters <- function(x){
  
  digits <- c()
  
  # TODO: punch this up.
  
  while(x){
    
    y <- ifelse(x %% 26 == 0, 26, x %% 26)
    
    digits <- c(digits, LETTERS[y])
    
    x <- (x - y) / 26
    
  }
  
  return(paste(rev(digits), collapse = ""))
  
}
