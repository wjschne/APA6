#' Interaction term replace
#'
#' Replaces ":" in interaction terms with the LaTeX multiply symbol
#' @param term A vector of effect names
#' @export
#' @importFrom magrittr %>%

termCleaner <- function(term) {
  out.format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (is.null(out.format)) out.format <- "html"
  x <- term %>% gsub(pattern = "(\\w)(\\w*)",
                     replacement = "\\U\\1\\E\\2",
                     perl = TRUE) %>%
    gsub(pattern = "I\\(",
         replacement = "") %>%
    gsub(pattern = "\\(",
         replacement = "") %>%
    gsub(pattern = "\\)",
         replacement = "") %>%
    gsub(pattern = "\\(Intercept\\)",
         replacement = "Intercept") %>%
    gsub(pattern = ":",
         replacement = ifelse( out.format == "latex", "~\\\\texttimes~", "&nbsp;&times;&nbsp;")) %>%
    gsub(pattern = "`",
         replacement = "")
  if (out.format != "latex") paste0(x,ifelse(regexpr("\\^",x) > 0, "^","")) else x %>% sub(pattern = "\\^",replacement = "$^{") %>% paste0(ifelse(regexpr("\\^",x) > 0, "}$",""))
}


