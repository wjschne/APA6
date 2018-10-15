#' APA Style P-value
#'
#' P-values are formatted to APA format from a numeric value using pvalueAPA
#' @param p a numeric value between 0 and 1
#' @param inline If TRUE returns LaTeX equation (e.g., $p=.02$). If false (default), only the number is returned.
#' @param mindigits The minimum number digits for rounding.
#' @param maxdigits The maximum number of digits for round small numbers.
#' @return APA formatted p-value
#' @export

pvalueAPA <- function(p, inline = FALSE, mindigits = 2, maxdigits = 3){
  fp <- function(x){
    if (is.na(x)) return(NA)
    p.round <- ifelse(x > 0.5 * 10 ^ (-1 * mindigits),mindigits,maxdigits)
    if (x > 0.5 * 10 ^ (-1 * p.round)) {
      paste0(ifelse(inline,"$p=", ""),
             sub(pattern = "0\\.",
                 replacement = ".",
                 formatC(x, p.round, format = "f")),
             ifelse(inline,"$", ""))
    } else {
      paste0(ifelse(inline, "$p<","<"),
             sub(pattern = "0\\.",
                 replacement =  ".",
                 10 ^ (-1 * maxdigits)),
             ifelse(inline,"$",""))
    }
  }
  sapply(p, fp)
}
