#' APA Correlation Table
#'
#' APA correlation tables are created from data frames using CorTable
#' @param x A matrix or data frame with numeric values.
#' @param caption Caption for the correlation table. Defaults to "".
#' @param alpha A numeric value to specify the largest
#' @return APA correlation table
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
    # Number of variables
    k <- ncol(x)
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
    # Phantom - for alignment
    d_cor <- matrix(formatC(R,
                            digits = digits,
                            format = "f"),
                    nrow = k)
    # Remove leading zeros
    d_cor <- sub("0.",".",d_cor)
    # Add bolding to significant correlations
    d_cor <- matrix(paste0(ifelse(p_bold,
                                  paste0("\\multicolumn{1}{B{.}{.}{0.",
                                         digits,
                                         "}}{"),
                                  ""),
                           d_cor,
                           ifelse(p_bold,"}","")),
                    nrow = k)
    # Remove upper triangle of matrix
    d_cor[upper.tri(d_cor, diag = TRUE)] <- ""
    # Remove NA
    d_cor[is.na(R)] <- ""
    # Remove last column
    d_cor <- d_cor[,-1*k]
    # Calculate row means/standard deviations
    vMeans <- apply(x,2,mean, na.rm = TRUE)
    isMeansNegative <- min(vMeans) < 0
    # Round to two decimal places
    vMeans <- matrix(paste0(formatC(vMeans,
                                    digits = digits,
                                    format = "f")),
                     ncol = 1)
    vMeansWidth <- max(nchar(vMeans))
    # Calculate row standard deviations
    vSDs <- matrix(formatC(
      apply(x,2,sd, na.rm = TRUE),
      digits = digits,
      format = "f"),
      ncol = 1)
    vSDsWidth <- max(nchar(vSDs))
    # Restructure data frame for LaTex code
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
    cat(paste0(
      paste0("\\newcolumntype{d}{D{.}{.}{0.",
             digits,
             "}}\n"),
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
                    paste0("D{.}{.}{",
                           vMeansWidth - digits - ifelse(isMeansNegative,0,1),
                           ".",
                           digits,
                           "}D{.}{.}{",
                           vSDsWidth - digits - 1,
                           ".",
                           digits,
                           "}"),
                    ""),
             " * {",
             k - 1,
             "}{d}}"),
      "\n\\toprule\n\\multicolumn{1}{r}{} & ",
      ifelse(is.null(vNames),"","& "),
      ifelse(includeMeanSD,
             "\\multicolumn{1}{c}{Mean} & \\multicolumn{1}{c}{SD} & ",
             ""),
      paste0("\\multicolumn{1}{>{\\centering\\arraybackslash}X}{",
            seq(1,k - 1),
            "}",
            collapse = " & "),
      "\\\\\n",
      "\\toprule ",
      tableCor,
      "\\toprule",
      "\n\\end{tabularx}",
      ifelse(notes == "",
             "",
             paste0("\n\n\\emph{Note}: ",notes)),
      "\n\\end{table}",
      ifelse(landscape,"\n\\end{landscape}",""),
      sep = "\n "))
  }
