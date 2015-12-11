#' Interaction term replace
#'
#' Replaces ":" in interaction terms with the LaTeX multiply symbol
#' @param x A vector of effect names
#' @export

InteractionReplace <- function(x){
  x <- gsub(x = x,
       pattern = ":",
       replacement = "LatexMultiply")
  x <- gsub(x = x, pattern = "[[:punct:]]",
         replacement = "")
  x <- gsub(x = x, pattern = "LatexMultiply",
         replacement = " $\\\\times$ ")
}
