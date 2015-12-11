#' APA ANOVA Table
#'
#' APA ANOVA tables are created from data frames using aovTable
#' @param m A model of class `aov`
#' @param caption A character string
#' @param partial_etasq Include partial eta squared
#' @param eta Include eta squared
#' @return APA ANOVA table
#' @export

aovTable <- function(m,caption = "ANOVA Table", partial_etasq = TRUE, eta = FALSE, ...){
  require(broom) # For tidying results
  require(lsr) # For effect sizes
  require(pander) # For table output
  require(dplyr) # For aggregating data
  # Tidy model
  tm <- tidy(m)
  # Create totals
  tr <- tm %>%
    select(df:sumsq) %>%
    summarize(df = sum(df), sumsq = sum(sumsq)) %>%
    mutate(statistic = NA, p.value = NA,term = "Total", meansq = sumsq/df)
  tm <- rbind(tm,tr)
  # Calculate and format effect sizes
  es <- etaSquared(m)
  es <- as.data.frame(es)  %>%  mutate(term = rownames(es))
  tm <- tm %>% left_join(es, "term")
  # Format table
  ftm <- tm %>% select(-term,-df) %>% apply(FUN = formatC, MARGIN = 2, digits = 2, format = "f")
  ftm[ftm == " NA"] <- ""
  ftm <- tm %>% select(df) %>% cbind(ftm)
  ftm$p.value <- pvalueAPA(tm$p.value)
  # Table headers
  rownames(ftm) <-  InteractionReplace(tm$term)
  rownames(ftm)[nrow(tm) - 1] <- "Error"
  cnames <- c("df","SS","MS","F","p")
  if (eta) cnames[length(cnames) + 1] <- "\\eta^2"   else ftm <- ftm %>% select(-eta.sq)
  if (partial_etasq) cnames[length(cnames) + 1] <- "\\eta_p^2"else ftm <- ftm %>% select(-eta.sq.part)
  colnames(ftm) <- paste0("$",cnames,"$")
  justifyString = paste0("l", paste0(rep("r",ncol(ftm)), collapse = ""), collapse = "")
  # Output table
  return(pander(ftm,
         missing = "",
         round = 2,
         keep.trailing.zeros = TRUE,
         emphasize.rownames = FALSE,
         justify = justifyString,
         caption = caption,
         ...))
}
