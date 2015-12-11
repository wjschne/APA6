CanCorTable <- function(x) {
  require(candisc)
  require(pander)
  require(magrittr)
  if (class(x) != "cancor") return("Object must be of class cancor from the candisc package.")
  k <- length(x$cancor)
  Eigenvalue = x$cancor ^ 2 / (1 - x$cancor ^ 2)
  EigPercent <- 100 * Eigenvalue / sum(Eigenvalue)
  data.frame(Variate = 1:k, 
             CanR = x$cancor, 
             CanRSq = x$cancor ^ 2, 
             Eigenvalue, 
             Percent = EigPercent, 
             CumulativePercent = cumsum(EigPercent)) %>% 
    set_colnames(c("Variate", 
                   "Canonical $R$",
                   "Canonical $R^2$",
                   "Eigenvalues",
                   "Percent",
                   "Cumulative Percent")) %>% 
    pander(caption = "Canonical Correlation Analysis")
}

CanCorTestTable <- function(x) {
  require(candisc)
  require(pander)
  require(magrittr)
  if (class(x) != "cancor") return("Object must be of class cancor from the candisc package.")
  k <- length(x$cancor)
  cbind(Variate = 1:k, Wilks(x)) %>% 
    mutate(p.value = pvalueAPA(p.value)) %>%
    set_colnames(c("Variate", 
                   "Canonical $R$", 
                   "Wilks $\\Lambda$", 
                   "$F$", 
                   "$df_1$",
                   "$df_2$",
                   "$p$")) %>% 
    pander(caption = "Test of H~0~: The canonical correlations in the current row and all that follow are zero.")
}

CanCorCoefTable <- function(x, 
                            type = c("raw",
                                     "standardized",
                                     "structure"), 
                            xNames = x$names$X, 
                            yNames = x$names$Y, 
                            xSet = "X", 
                            ySet = "Y",
                            boldthreshold = 0.3,
                            digits = 2) {
  require(candisc)
  require(pander)
  require(magrittr)
  bolder <- function(p, 
                     boldthreshold = boldthreshold, 
                     digits = digits) {
    p0 <- formatC(p,digits = digits, format = "f")
    if (abs(p) > boldthreshold) paste0("**",p0 ,"**") else p0}
  
  k <- length(x$cancor)
  XTitleRow <- matrix(rep(NA,k),nrow = 1) %>%
    set_rownames(paste0("**",xSet,"**"))
  YTitleRow <- matrix(rep(NA,k),nrow = 1) %>% set_rownames(paste0("**",ySet,"**"))
  if (type == "structure") {
    xcoef <- x$structure$X.xscores %>%
      set_rownames(xNames) %>% 
      apply(MARGIN = c(1,2),
            bolder, 
            boldthreshold = boldthreshold, 
            digits = digits)
    ycoef <- x$structure$Y.yscores %>%
      set_rownames(yNames) %>% 
      apply(MARGIN = c(1,2),
            bolder, 
            boldthreshold = boldthreshold, 
            digits = digits)
    cap <- "Canonical Structure Coefficients"
  } 
  
  if (type == "raw") {
    xcoef <- x$coef$X %>%
      set_rownames(xNames)
    ycoef <- x$coef$Y %>%
      set_rownames(yNames)
    cap = "Canonical Function Coefficients"
  }
  
  if (type == "standardized") {
    xcoef <- coef(x, 
                  type = "x", 
                  standardize = TRUE) %>% 
      set_rownames(xNames) %>% 
      apply(MARGIN = c(1,2), 
            bolder, 
            boldthreshold = boldthreshold, 
            digits = digits)
    ycoef <- coef(x, 
                  type = "y", 
                  standardize = TRUE) %>% 
      set_rownames(yNames) %>% 
      apply(MARGIN = c(1,2), 
            bolder, 
            boldthreshold = boldthreshold, 
            digits = digits)
    cap <- "Standardized Canonical Function Coefficients"
  }
  rbind(XTitleRow,
        xcoef,
        YTitleRow,
        ycoef) %>%
    set_colnames(paste0("CV",1:k)) %>% 
    pander(caption = cap,
           justify = paste0("l",
                            paste0(rep("r",k),
                                   collapse = "")))
}