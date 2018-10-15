#' Format numbers for tables by replacing the hyphen with a minus sign.
#'
#' @param x A numeric variable.
#' @param digits Number of digits after the decimal place. Default = 2
#' @param stripzero Remove the leading zero? Default = FALSE
#' @param addDollars Add dollar signs for latex math mode
#'
#' @export


tabnum <- function(x,digits = 2, stripzero = FALSE, addDollars = FALSE) {
  out.format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  output <- formatC(x, digits, format = "f")
  if (stripzero) output <- sub("^-0\\.","-\\.",sub("^0\\.","\\.",output))
  if (is.null(out.format)) {

  } else {
    if (out.format == "latex") {
      if (addDollars) {
        dollarMe <- function(column) paste0("$",column,"$")
        if (!is.null(dim(x))) output <- apply(output,2,dollarMe) else output <- dollarMe(output)
      }

    } else {
      output <- sub("-", "&minus;", output)
    }
  }
  output[is.na(x)] <- NA
  return(output)
}


