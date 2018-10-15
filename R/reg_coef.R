#' Regression Analysis Summary Table
#'
#' @param m An object of class lm.
#' @param caption A character string denoting the title of the regression table.
#' @param digits Number of digits after the decimal place. Defaults to 2.
#' @param level Set the confidence interval. Defaults to 0.95.
#' @param markdown Return a markdown table. Defaults to TRUE.
#' @param unstandardized Include unstandardized estimates. Defaults to TRUE.
#' @param se Include standard error. Defaults to TRUE.
#' @param ci Include confidence interval. Defaults to TRUE.
#' @param t.value Include t-values. Defaults to TRUE.
#' @param p.value Include p-values. Defaults to TRUE.
#' @param standardized Include standardized estimates. Defaults to TRUE.
#' @param zero.order.cor Include zero-order correlations. Defaults to FALSE.
#' @param structure.coef Include structure coefficients. Defaults to FALSE.
#' @param partial.cor Include partial correlations. Defaults to FALSE.
#' @param semipartial.cor Include semi-partial correlations. Defaults to FALSE.
#' @param semipartial.cor.squared Include semi-partial correlations squared. Defaults to TRUE
#' @param product.measure Include the product measure (zero-order correlation * beta)
#'
#' @importFrom broom tidy
#' @importFrom dplyr select
#' @importFrom lm.beta lm.beta
#' @importFrom magrittr %>%
#' @importFrom pander pander
#' @importFrom ppcor pcor
#' @importFrom ppcor spcor
#' @importFrom rmarkdown latex_dependency
#' @importFrom knitr asis_output
#' @importFrom stats model.matrix
#' @importFrom stats anova
#' @importFrom stats cor
#' @importFrom stats confint
#' @importFrom stats coef
#'
#' @export

reg_coef <-
  function(m,
           caption = "Regression Analysis Summary",
           digits = 2,
           level = 0.95,
           markdown = TRUE,
           unstandardized = TRUE,
           se = TRUE,
           ci = TRUE,
           t.value = TRUE,
           p.value = TRUE,
           standardized = TRUE,
           zero.order.cor = FALSE,
           product.measure = FALSE,
           structure.coef = FALSE,
           partial.cor = FALSE,
           semipartial.cor = FALSE,
           semipartial.cor.squared = TRUE) {
    # # test model
    # n <- 10
    # X1 <- rnorm(n)
    # X2 <- rnorm(n)
    # X3 <- rnorm(n)
    # Y <- sin(X1) + -1*sin(X2) + sin(X3) + rnorm(n, 5, 0.5)
    # m <- lm(Y ~ X1 * X2 + X3 + I(X3^2))
    # library(broom)
    # library(ppcor)
    # library(dplyr)
    # library(lm.beta)
    # library(magrittr)
    # library(pander)
    # caption = "Regression Analysis Summary"
    # digits = 2
    # unstandardized = TRUE
    # se = TRUE
    # ci = TRUE
    # level = 0.95
    # standardized = TRUE
    # t.value = TRUE
    # p.value = TRUE
    # zero.order.cor = FALSE
    # structure.coef = FALSE
    # partial.cor = FALSE
    # semipartial.cor = FALSE
    # semipartial.cor.squared = TRUE
    # product.measure = FALSE



    # Format outcome -----------
    out.format <- knitr::opts_knit$get("rmarkdown.pandoc.to")
    if (is.null(out.format)) out.format <- "other"
    bLatex <- out.format == "latex"

    # column parameters -----------
    prows <- data.frame(
      parameter = c(
        "unstandardized",
        "se",
        "ci",
        "t.value",
        "p.value",
        "standardized",
        "zero.order.cor",
        "product.meausure",
        "structure.coef",
        "partial.cor",
        "semipartial.cor",
        "semipartial.cor.squared"
      ),
      include = c(
        unstandardized,
        se,
        ci,
        t.value,
        p.value,
        standardized,
        zero.order.cor,
        product.measure,
        structure.coef,
        partial.cor,
        semipartial.cor,
        semipartial.cor.squared
      ), stringsAsFactors = FALSE
    )

    ptable <- merge(parameterTable, prows, by = "parameter",sort = FALSE)
    ptable <- ptable[ptable$include,]

    # tidied coefficients
    mtidy <- tidy(m)
    termNames <- mtidy$term %>% termCleaner()

    # Regression coefficient table -------
    values_table <- data.frame(term = termNames,
                               stringsAsFactors = FALSE)

    # unstandardized estimates ------------
    if (unstandardized) {
      values_table$unstandardized <-
        mtidy$estimate %>% tabnum(digits = digits)
    }


    # SE of B
    if (se) {
      values_table$se <- mtidy$std.error %>%
        tabnum(digits = digits)
    }

    # Confidence Intervals
    if (ci) {
      ci_old <- m %>%
        confint(level = level)
      colnames(ci_old) <- c("LB", "UB")
      ci_old <- tabnum(ci_old)
      if (bLatex) {
        ci_new <-
          paste0("[", apply(ci_old, 1, paste0, collapse = ",~"), "]")
        ci_label <-
          paste0("$", formatC(level * 100, digits = 0, format = "f"), "\\%~CI$")
        ptable[ptable$parameter == "ci", "tableColnamesLatex"] <-
          ci_label
      } else {
        ci_new <-
          paste0("[", ci_old[, 1], ",&nbsp;", ci_old[, 2], "]", sep = "")
        ci_label <-
          paste0(formatC(level * 100, digits = 0, format = "f"), "% CI")
        ptable[ptable$parameter == "ci", "tableColnames"] <-
          ci_label
      }
      values_table$ci <- ci_new
    }

    # t-values
    if (t.value) {
      values_table$t.value <-
        mtidy$statistic %>% tabnum(digits = digits)
    }


    # p-values
    if (p.value) {
      values_table$p <- mtidy$p.value %>% pvalueAPA
      if (out.format == "html") {
        values_table$p <- sub("<","&lt;",values_table$p,fixed = TRUE)
      }
    }


    # standardized estimates -------------------
    betas <- m %>%
      lm.beta %>%
      coef
    betas[1] <- NA
    if (standardized) {
      values_table$betas <- betas %>%
        tabnum(digits = digits, stripzero = TRUE)
    }

    # y-hats plus predictor values
    md <- cbind(m$model[, 1, drop = FALSE], model.matrix(m)[, -1])

    # zero-order correlations
    zeroorder <- c(NA, cor(md)[-1, 1])
    if (zero.order.cor) {
      values_table$zeroorder <- zeroorder %>%
        tabnum(digits = digits, stripzero = TRUE)
    }

    # product measure
    if (product.measure) {
      values_table$product.measure <-
        (zeroorder * (m %>% lm.beta %>% coef)) %>%
        tabnum(digits = digits, stripzero = TRUE)
    }

    # structure coefficients
    if (structure.coef) {
      values_table$structurecoef <-
        (zeroorder / sqrt(summary(m)$r.squared)) %>%
        tabnum(digits = digits, stripzero = TRUE)
    }

    # partial correlations ------------
    if (partial.cor) {
      values_table$partialcor <- c(NA, pcor(md)$estimate[1, -1]) %>%
        tabnum(digits = digits, stripzero = TRUE)
    }

    # semi-partial correlations
    semipartialcor <- c(NA, spcor(md)$estimate[1, -1])
    if (semipartial.cor) {
      values_table$semipartialcor <- semipartialcor %>%
        tabnum(digits = digits, stripzero = TRUE)
    }

    # semi-partial coefficients squared
    if (semipartial.cor.squared) {
      values_table$sp2 <- (semipartialcor ^ 2) %>%
        tabnum(digits = digits, stripzero = TRUE)
    }


    # Set column alignment ------

    justifyString <-
      paste0("l", paste0(ptable$alignment, collapse = ""))

    rownames(values_table) <- values_table$term
    values_table <- values_table[,-1]

    dcj <- paste0("D{.}{.}{",apply(values_table,2,dcolumn_format),"}")
    names(dcj) <- colnames(values_table)
    if (ci) dcj["ci"] <- paste0("D{,}{,}{",dcolumn_format(values_table[,"ci"],","),"}")

    # set column names
    if (bLatex) {
      colnames(values_table) <- ptable$tableColnamesLatex
    } else {
      colnames(values_table) <- ptable$tableColnames
    }



    # note values
    rsquared <-  summary(m)$r.squared %>% tabnum(digits = digits,stripzero = TRUE) %>% gsub(pattern = "\\$",replacement = "")
    SEE <- summary(m)$sigma %>% tabnum()

    n <-  m$model %>% nrow

    if (bLatex) {
      p <- anova(m)$'Pr(>F)'[1] %>%
        pvalueAPA(0.0005, inline = TRUE)
      note <-
        paste0(
          "\\emph{Note}. $\\sigma_e = ",
          SEE, "$, ",
          "$R^2 = ",
          rsquared,
          "$ ($N$ = ",
          n,
          ", ",
          p,
          "). ",
          paste0(ptable$notesLatex, collapse = " ")
        )
    } else {
      p <- anova(m)$'Pr(>F)'[1] %>%
        pvalueAPA(0.0005, inline = TRUE) %>%
        gsub(pattern = "\\$", replacement = "") %>%
        gsub(pattern = "=", replacement = " = ") %>%
        gsub(pattern = "<", replacement = " &lt; ") %>%
        gsub(pattern = "p", replacement = "*p*")
      note <-
        paste0(
          "*Note*. *&sigma;~e~* = ",
          SEE,
          ", *R*^2^ = ",
          rsquared,
          " (*N* = ",
          n,
          ", ",
          p,
          "). ",
          paste0(ptable$notes, collapse = " ")
        )
    }


    # pander table --------
    if (ncol(values_table) == 0) {
      "No columns selected to print in summary table"
    } else if (markdown) {
      if (bLatex) {
        values_table[is.na(values_table)] <- "~"
        tablebody <- paste0(apply(
          cbind(termNames, values_table), 1, paste0, collapse = " & "
        ), collapse = " \\tabularnewline\n")

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
            paste0("l",paste0(dcj,collapse = "")),
            "}\n",
            "\\captionsetup{singlelinecheck=off}\n",
            "\\caption{",
            caption,
            "}\\tabularnewline\n",
            "\\toprule\n",
            paste0(c("~", paste0("\\multicolumn{1}{c}{",colnames(values_table),"}")), collapse = " & "),
            "\\tabularnewline\n",
            "\\midrule\n",
            "\\endfirsthead\n",
            "\\toprule\n",
            paste0(c("~", paste0("\\multicolumn{1}{c}{",colnames(values_table),"}")), collapse = " & "),
            "\\tabularnewline\n",
            "\\midrule\n",
            "\\endhead\n",
            "\\insertTableNotes\n",
            "\\endlastfoot\n",
            tablebody,
            "\\tabularnewline\n",
            "\\midrule\n",
            "\\end{longtable}\n",
            "\\end{ThreePartTable}"
          ),
          meta = list(
            latex_dependency("longtable"),
            latex_dependency("caption"),
            latex_dependency("booktabs"),
            latex_dependency("dcolumn"),
            latex_dependency("threeparttable",options = "flushleft"),
            latex_dependency("threeparttablex")
          )
        ))
      } else {
        values_table[is.na(values_table)] <- "&nbsp;"
        if (out.format == "html") {
          tdtag <- function(x) paste0("<td>",x,"</td>")
          trtag <- function(x) paste0("<tr>",x,"</tr>")



          tablebody <- cbind(termNames,
                             values_table) %>%
            apply(c(1,2),tdtag) %>%
            apply(1, paste0, collapse = "") %>%
            trtag %>%
            paste0(collapse = "\n")

          asis_output(
            paste0(
              "<style  type='text/css'>\n",
              ".APATable  {border-collapse:collapse;border-spacing:0;}\n",
              ".APATable td{padding:5px;text-align:right;}\n",
              ".APATable tbody {border-bottom: 1px solid black;}\n",
              ".APATable thead {border-bottom: 1px solid black;border-top:1px solid black;}\n",
              ".APATable td:nth-child(1) {text-align:left}\n",
              "</style>\n",
              "<table class='APATable'>\n",
              "<caption style = 'padding:5px 5px;text-align:left;color:black'>",
              caption,
              "</caption>\n",
              "<thead>\n",
              "<tr>\n",
              "<td>&nbsp;</td>",
              paste0("<td>",
                     colnames(values_table),
                     "</td>",
                     collapse = "\n"),
              "</tr>\n",
              "</thead>\n",
              "<tbody>\n",
              tablebody,
              "</tbody>\n",
              "<tfoot>\n",
              "<tr>\n",
              "<td colspan ='",
              ncol(values_table) + 1,
              "'>",
              note,
              "</td>",
              "</tr>\n",
              "</tfoot>",
              "</table>",
              collapse = "")
          )
        } else {
          pander(
            values_table,
            caption = paste0(caption,
                             "\n\n",
                             note,
                             collapse = "\n"),
            round = digits,
            split.tables = Inf,
            style = "rmarkdown",
            emphasize.rownames = FALSE,
            table.alignment.rownames = "left",
            missing = "",
            justify = justifyString
          )
        }




      }
    } else {
      values_table
    }

  }
