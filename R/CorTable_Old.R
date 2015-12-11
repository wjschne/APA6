#' APA Correlation Table
#'
#' APA correlation tables are created from data frames using CorTable
#' @param x A data frame with numeric values
#' @return APA correlation table
#' @export

CorTable_Old <-
  function(x, caption = "", notes = "", alpha = 0.05, digits = 2, boldSig = TRUE, includeMeanSD = TRUE, includeCaptionNumber = TRUE, WidthProportion = 1, use = "pairwise.complete.obs", landscape = FALSE){
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
    d_cor <- matrix(paste0(ifelse(R < 0,
                                  "",
                                  "\\phantom{-}"),
                           formatC(R,
                                   digits = digits,
                                   format = "f"),
                           "\\phantom{-}"),
                    nrow = k)
    # Add bolding to significant correlations
    d_cor <- matrix(paste0(ifelse(p_bold,"$\\mathbf{","$"),d_cor,ifelse(p_bold,"}$","$")), nrow = k)
    # Remove upper triangle of matrix
    d_cor[upper.tri(d_cor, diag = TRUE)] <- ""
    # Remove leading zeros
    d_cor <- sub("0.",".",d_cor)
    d_cor <- d_cor[,-1*k]
    # Calculate row means/standard deviations
    vMeans <- apply(x,2,mean, na.rm = TRUE)
    # Round to two decimal places
    vMeans <- matrix(paste0("$",formatC(vMeans,
                                        digits = digits,
                                        format = "f"),"$"), ncol = 1)
    # Calculate row standard deviations
    vSDs <- matrix(formatC(
      apply(x,2,sd, na.rm = TRUE),
      digits = 2,
      format = "f"),
      ncol = 1)
    # Restructure data frame for LaTex code
    tableCor <- character(0)
    for (i in 1:k) {
      tableCor <- paste0(tableCor,i,". & ", vNames[i], " & ", ifelse(includeMeanSD, paste0(vMeans[i], " & ", vSDs[i], " & "),""), paste0(paste(d_cor[i,],collapse = " & "),"\\\\\n"))
    }
    # Build LaTex Table
    cat(paste0(
      ifelse(landscape,"\\begin{landscape}",""),
      "\n\\begin{table}[h]",
      "\n\\caption",
      ifelse(includeCaptionNumber,"","*"),
      "{",caption,"}",
      "\n\\begin{tabularx}{",
      WidthProportion,
      "\\textwidth}",
      paste0("{ll",
             ifelse(includeMeanSD,"rr",""),
             " * {",
             k - 1,
             "}{>{\\centering\\arraybackslash}X}}"),
      "\n\\toprule & & ",
      ifelse(includeMeanSD,
             "Mean & SD &",
             ""),
      paste(seq(1,k - 1), collapse = " & "),
      "\\\\\n",
      "\\toprule ",
      tableCor,
      "\\toprule",
      "\n\\end{tabularx}",
      ifelse(notes == "",
             "",
             paste0("\n\n\\emph{Note}: ",notes)),
      "\n\\end{table}",
      ifelse(landscape,"\\end{landscape}",""),
      sep = "\n "))
  }
