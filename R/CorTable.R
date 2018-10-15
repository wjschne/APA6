#' APA Correlation Table
#'
#' APA correlation tables are created from data frames using CorTable
#' @param x A matrix or data frame with numeric values.
#' @param caption Caption for the correlation table. Defaults to "".
#' @param alpha A numeric value to specify the significance level.
#' @param notes Table notes.
#' @param digits Number of digits after the decimal place.
#' @param boldSig Will bold significant correlations.
#' @param includeMeanSD Will include mean and standard deviations.
#' @param includeCaptionNumber Will include a caption number.
#' @param WidthProportion Adjusts the width of the table.
#' @param use Method for dealing with missing values.
#' @param landscape Makes page landscape orientation.
#' @return APA correlation table
#' @importFrom rmarkdown latex_dependency
#' @importFrom stats pt
#' @importFrom stats sd
#' @export


CorTable <-
  function(x,
           caption = "",
           notes = "",
           alpha = 0.05,
           digits = 2,
           boldSig = TRUE,
           includeMeanSD = TRUE,
           includeCaptionNumber = TRUE,
           WidthProportion = 1,
           use = "pairwise.complete.obs",
           landscape = FALSE){

    out.format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
    if (is.null(out.format)) out.format <- "other"
    out.format <- 'latex'

    # Number of variables
    k <- ncol(x)

    # assign variable names, if missing
    if (is.null(colnames(x))) colnames(x) <- paste0("V",1:k)


    # Correlation matrix
    R <- cor(x, use = use)
    # Variable names
    vNames <- colnames(x)
    # Sample size
    N <- nrow(x)
    # Degrees of freedom
    df <- N - 2
    # t-statistic matrix
    t <-  R * sqrt(df / (1 - R ^ 2))
    # P-value matrix
    p <- pt(-1*abs(t), df) * 2
    # Bold significant correlations
    p_bold <- (p < alpha) & boldSig
    diag(p_bold) <- FALSE
    # Round to significant digits
    d_cor <- matrix(formatC(R,
                            digits = digits,
                            format = "f"),
                    nrow = k)
    # Remove leading zeros
    d_cor <- sub("0[.]",".",d_cor)

    # Add bolding to significant correlations
    latexBolder <- function(x, bold = matrix(rep(FALSE,length(c(x))),ncol = ncol(x))) {
      x1 <- ifelse(as.numeric(x) == 0,paste0(".",paste0(rep("0",digits),collapse = "")),x)
      x2 <- ifelse(bold,paste0(sub("."," & \\boldmath{$.",sub("-","\\boldmath{$-$}",x,fixed = TRUE), fixed = TRUE),"$}"),sub("-","$-$",sub("."," & .",x1,fixed = TRUE),fixed = TRUE))
      dim(x2) <- c(nrow(x),ncol(x))
      x2
    }


    if (out.format == "latex") {
      d_cor <- latexBolder(d_cor,p_bold)

      # Remove upper triangle of matrix
      d_cor[upper.tri(d_cor, diag = TRUE)] <- "~ & ~"
      # Remove NA
      d_cor[is.na(R)] <- "~ & ~"

    } else {
      d_cor <- matrix(paste0(ifelse(p_bold,
                                    paste0("**",d_cor,"**"),
                                    d_cor)),
                      nrow = k)
      # Remove upper triangle of matrix
      d_cor[upper.tri(d_cor, diag = TRUE)] <- ""
      # Remove NA
      d_cor[is.na(R)] <- ""
    }


    # Remove last column
    d_cor <- d_cor[,-1*k,drop = FALSE]
    # Calculate row means/standard deviations
    vMeans <- apply(x,2,mean, na.rm = TRUE)
    isMeansNegative <- min(vMeans) < 0
    # Round to two decimal places
    vMeans <- matrix(paste0(formatC(vMeans,
                                    digits = digits,
                                    format = "f")),
                     ncol = 1)
    vMeansWidth <- max(nchar(vMeans))
    vMeans <- latexBolder(vMeans)

    # Calculate row standard deviations
    vSDs <- matrix(formatC(
      apply(x,2,sd, na.rm = TRUE),
      digits = digits,
      format = "f"),
      ncol = 1)
    vSDs <- latexBolder(vSDs)
    vSDsWidth <- max(nchar(vSDs))
    # Restructure data frame for LaTex code


    if (includeMeanSD) {
      tablebody <- cbind(vMeans,vSDs,d_cor)
    } else tablebody <- d_cor
    tablebody <- apply(tablebody,1,paste0,collapse = " & ")
    tablebody <- paste0(tablebody,"\\",collapse = "\n")
    tableCor <- character(0)
    for (i in 1:k) {
      tableCor <- paste0(tableCor,
                         i,
                         ifelse(is.null(vNames),
                                "",
                                paste0(" & ",
                                       vNames[i])),
                         " & ",
                         ifelse(includeMeanSD,
                                paste0(vMeans[i],
                                       " & ",
                                       vSDs[i],
                                       " & "),
                                ""),
                         paste0(paste(d_cor[i,],
                                      collapse = " & "),
                                "\\\\\n"))
    }
    # Build LaTex Table
    return(asis_output(
      paste0(
      paste0(
        ifelse(landscape,"\\begin{landscape}",""),
        "\n\\begin{table}[h]",
      "\n\\caption",
      ifelse(includeCaptionNumber,"","*"),
      "{",caption,"}",
      "\n\\begin{tabularx}{",
      WidthProportion,
      ifelse(landscape,"\\textheight}","\\textwidth}"),
      paste0("{r@{.~}",
             ifelse(is.null(vNames),"","l"),
             ifelse(includeMeanSD,
                    "r@{}lr@{}l",
                    ""),
             " * {",
             k - 1,
             "r@{}l}"),
      "\n\\toprule\n\\multicolumn{1}{r}{} & ",
      ifelse(is.null(vNames),"","& "),
      ifelse(includeMeanSD,
             "\\multicolumn{2}{c}{Mean} & \\multicolumn{2}{c}{SD} & ",
             ""),
      paste0("\\multicolumn{2}{>{\\centering\\arraybackslash}X}{",
            seq(1,k - 1),
            "}",
            collapse = " & "),
      "\\\\\n",
      "\\toprule ",
      tablebody,
      "\\toprule",
      "\n\\end{tabularx}",
      ifelse(notes == "",
             "",
             paste0("\n\n\\emph{Note}: ",notes)),
      "\n\\end{table}",
      ifelse(landscape,"\n\\end{landscape}",""),
      sep = "\n "),
      ),
      meta = list(
        latex_dependency("tabularx"),
        latex_dependency("caption"),
        latex_dependency("booktabs"),
        latex_dependency("pdflscape"),
        latex_dependency("mathptmx"))))
  }
