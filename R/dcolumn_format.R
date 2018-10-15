#' Format for dcolumn
#'
#' Width of column to the right and left of decimal
#' @param x a character vector
#' @param splitter the character on which to align the column
#' @export
#' @importFrom magrittr %>%

dcolumn_format <- function(x, splitter = "."){
  x[is.na(x)] <- "."
  gsub("-","",x) %>% # remove minus signs
    strsplit(splitter,fixed = TRUE) %>% # split strings
    lapply(nchar) %>% # count characters of each string
    as.data.frame %>%
    apply(1,max) %>% # find the largest count
    paste0(collapse = ".") # collapse string
}


