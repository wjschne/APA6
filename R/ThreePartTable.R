#' Three-part tables (Caption-body-note)
#'
#' @param tablebody A matrix or data.frame of the table.
#' @param caption A character string denoting the title of the table.
#' @param note A character string denoting the note at the foot of the table.
#' @param align A character string to align columns: l = left, c = center, r = right, d = decimal e.g., align = "llccrrdd"
#' @param digits A numeric value indicating the number of digits to round numeric columns
#'
#' @importFrom magrittr %>%
#' @importFrom pander pander
#' @importFrom rmarkdown latex_dependency
#' @importFrom knitr asis_output
#'
#' @export


ThreePartTable <- function(tablebody,
                           caption = "",
                           note = "",
                           align = NA,
                           digits = 2) {

  # Output format -----------
  out.format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (is.null(out.format)) out.format <- "other"

  # column classes
  columnclass <- sapply(tablebody,class)

  # table rows
  k <- nrow(tablebody)
  !is.na(as.numeric(c(NA,"2.1",'.35',"h-.5")))


  # justify columns ---------------
  if (is.na(align)) {
    justify <- columnclass
    justify[justify == "numeric"] <- "d"
    justify[justify == "character"] <- "l"
    justify[justify == "integer"] <- "c"
    justify[justify == "logical"] <- "c"
    justify[justify == "factor"] <- "l"

    if (!is.null(rownames(tablebody)) &&
        !all(as.character(1:k) == rownames(tablebody))) {
      justify <- c("l", justify)
    }
  } else {
    justify <- strsplit(align, split = "", fixed = T)[[1]]
  }


  # Add rownames to table
  if (!is.null(rownames(tablebody))) {
    if (!all(as.character(1:k) == rownames(tablebody)))
      tablebody <- cbind(rownames(tablebody), tablebody)
  }



  tableformatter <- function(x, type) {
    if (type == "numeric") {
      formatC(x, digits = digits, format = "f")
    } else {
      x
    }
  }

  tablebody <- mapply(tableformatter,tablebody,columnclass)


  if (out.format == "latex") {
    # latex headers ---------
    tablehead <-
      paste0(
        "\\multicolumn{1}{",
        ifelse(justify == "d", "c", justify),
        "}{",
        colnames(tablebody),
        "}",
        collapse = " & "
      )
    justify[justify == "d"] <- "D{.}{.}{2.2}"
    justifystring <- paste0(justify, collapse = "")
    # table body latex  -----------
    tablebodystring <- paste0(apply(tablebody,
                                    1,
                                    paste0,
                                    collapse = " & "),
                              "\\tabularnewline\n",
                              collapse = "")

    return(asis_output(
      paste0(
        "\\begin{ThreePartTable}\n",
        "\\begin{TableNotes}\n",
        "\\setlength\\labelsep{0pt}\n",
        "\\item ",
        note,
        "\n",
        "\\end{TableNotes}",
        "\\begin{longtable}[l]{",
        justifystring,
        "}\n",
        "\\captionsetup{singlelinecheck=off}\n",
        "\\caption{",
        caption,
        "}\\tabularnewline\n",
        "\\toprule\n",
        tablehead,
        "\\tabularnewline\n",
        "\\midrule\n",
        "\\endfirsthead\n",
        "\\toprule\n",
        tablehead,
        "\\tabularnewline\n",
        "\\midrule\n",
        "\\endhead\n",
        "\\insertTableNotes\n",
        "\\endlastfoot\n",
        tablebodystring,
        "\\midrule\n",
        "\\end{longtable}\n",
        "\\end{ThreePartTable}"
      ),
      meta = list(
        latex_dependency("longtable"),
        latex_dependency("caption"),
        latex_dependency("booktabs"),
        latex_dependency("dcolumn"),
        latex_dependency("threeparttable", options = "flushleft"),
        latex_dependency("threeparttablex")
      )
    )) }


  if (out.format == "html") {
    # column alignment html ----
    justify[justify == "d"] <- "right"
    justify[justify == "l"] <- "left"
    justify[justify == "c"] <- "center"
    justify[justify == "r"] <- "right"
    justifystring <- paste0(".APATable td:nth-child(",
                            1:length(justify),
                            ") {text-align:",
                            justify,
                            "}\n",
                            collapse = "")


    # table body html ---------
    tdtag <- function(x) paste0("<td>",x,"</td>")
    trtag <- function(x) paste0("<tr>",x,"</tr>")
    htmlminus <- function(x) if (type == "numeric" | type == "integer") sub("-","&minus;",x, fixed = TRUE) else x

    sapply(tablebody,class)

    tablebodystring <- apply(tablebody,c(1,2),tdtag)
    tablebodystring <- apply(tablebodystring, 1, paste0, collapse = "")
    tablebodystring <- trtag(tablebodystring)
    tablebodystring <- paste0(tablebodystring,collapse = "\n")
    tablehead <- colnames(tablebody)


    asis_output(
      paste0(
        "<style  type='text/css'>\n",
        ".APATable  {border-collapse:collapse;border-spacing:0;}\n",
        ".APATable td{padding:5px;text-align:right;}\n",
        ".APATable tbody {border-bottom: 1px solid black;}\n",
        ".APATable thead {border-bottom: 1px solid black;border-top:1px solid black;}\n",
        tablebodystring,
        "</style>\n",
        "<table class='APATable'>\n",
        "<caption style = 'padding:5px 5px;text-align:left;color:black'>",
        caption,
        "</caption>\n",
        "<thead>\n",
        "<tr>\n",
        paste0("<td>",
               tablehead,
               "</td>",
               collapse = "\n"),
        "</tr>\n",
        "</thead>\n",
        "<tbody>\n",
        tablebodystring,
        "</tbody>\n",
        "<tfoot>\n",
        "<tr>\n",
        "<td colspan ='",
        ncol(tablebody),
        "'>",
        note,
        "</td>",
        "</tr>\n",
        "</tfoot>",
        "</table>",
        collapse = "")
    )
  }
}
