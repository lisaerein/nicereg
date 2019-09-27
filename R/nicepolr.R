#' Lisa's Proportional Odds Logistic Regression Table Reporting Function
#'
#' This function creates a nice looking table of regression results for a polr (MASS) model.
#' Input either a polr object or dataframe, outcome variable, vector of covariates, method, and type of analysis (univariate or multiple regression).
#' The function returns a dataframe of formatted regression results and prints the results table using kable or htmlTable.
#' @param fit polr model object [fit or method/covs/out are REQUIRED].
#' @param df Dataframe [fit or df/method/covs/out].
#' @param method Character. Model type in quotes ("logisitc", "probit", "loglog", "cloglog", "cauchit"). Default is "logistic".
#' @param covs Character. Vector of covariates to include in model [fit or df/method/covs/out are REQUIRED].
#' @param out Character. Outcome for regression model [fit or df/method/covs/out are REQUIRED].
#' @param regtype Logical. Should the covariates be run separately ("uni") or together in a multiple regression model ("multi"). Deafult is "multi".
#' @param exp Logical. Option to exponentiate coefficients and CI's. Default is NA (estimates exponentiated for logistic method by default).
#' @param estname Character. Label for regression estimate. Default is NA (program will choose a reasonable name depending on model type).
#' @param refcat Logical. If TRUE the table will create a separate line for the reference category. Default is FALSE.
#' @param labels Character. Vector of labels for covariates. Default is NA (use variable names).
#' @param type2 Logical. If TRUE, type II p-values will be added to the table. Default is FALSE.
#' @param type3 Logical, If TRUE, type III p-values will be added to the table. Default is FALSE.
#' @param est.dec Numeric. Number of decimal places for estimates. Default is 2.
#' @param ci.dec Numeric. Number of decimal places for 95 percent confidence interval. Default is 2.
#' @param pval.dec Numeric. Number of decimal places for pvalues. Must be an integer from 1 to 4. Default is 3.
#' @param color Character. Color to use for htmlTable striping. Default is "#EEEEEE" (light grey). Use "white" for no striping.
#' @param kable Logical. If TRUE, table will be formatted using the kable packge. Default is TRUE.
#' @param htmlTable Logical. If TRUE, the table will be printed using htmlTable. Default is FALSE.
#' @param title Character. Optional title above table. Default is "".
#' @param footer Logical. If TRUE, table will include a footnote with model details. Default is TRUE.
#' @keywords glm table logistic poisson linear regression reporting
#' @importFrom knitr kable
#' @importFrom htmlTable htmlTable
#' @importFrom car Anova
#' @importFrom MASS polr
#' @export
nicepolr    <- function(fit = NA
                       ,df = NA
                       ,method = "logistic"
                       ,covs = NA
                       ,out = NA
                       ,regtype = "multi"
                       ,exp = NA
                       ,estname = NA
                       ,refcat = FALSE
                       ,labels = NA
                       ,type2 = FALSE
                       ,type3 = FALSE
                       ,est.dec = 2
                       ,ci.dec = 2
                       ,pval.dec = 3
                       ,color = "#EEEEEE"
                       ,kable = TRUE
                       ,htmlTable = FALSE
                       ,title = ""
                       ,footer = TRUE
                       ){
  
  ### set contrast settings - factor labels lost with non-default settings
  current_contr = options("contrasts")
  options(contrasts = c("contr.treatment","contr.poly"))
  
  # check user inputs -------------------------------------------------------
  
  if (!is.na(fit[1])) regtype <- "multi"
  try(if (is.na(fit[1]) & (is.na(regtype) | !(regtype %in% c("uni", "multi")))) stop("regtype must be uni or multi\n"))
  
  if (is.na(fit[1])){
    
    try(if (class(df) != "data.frame" | is.na(covs[1]) | is.na(out[1]) | is.na(method[1])) stop("must provide model object or dataframe + covariates + outcome + model method\n"))
    
    ## remove any covs that do not appear in dataset
    covs2 <- covs[covs %in% names(df)]
    if (length(covs2) != length(covs)) cat("Warning! Covariate(s):", covs[!(covs %in% names(df))],"do not exist in dataset\n")
    covs <- covs2
    try(if (length(covs) == 0) stop("No valid covs\n"))
    
    ## check that outcome appears in dataset
    out2 <- out[out %in% names(df)]
    try(if (length(out2) != 1) stop("Outcome: ", out[!(out %in% names(df))]," does not exist in dataset\n"))
    out <- out2
    
    try(if (class(covs[1]) != "character") stop("covs must be a character vector\n"))
    try(if (class(out[1]) != "character" | length(out) != 1) stop("out must be a single character string\n"))
    try(if (class(method[1]) != "character" | !method %in% c("logistic", "probit", "loglog", "cloglog", "cauchit")) stop ("invalid method see polr documentation\n"))
  }
  
  # make kable the default table format
  if (!kable & !htmlTable) kable <- TRUE
  
  # define formats and helper functions -------------------------------------
  
  trim <- function(x) gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
  
  simcap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
  }
  
  esformat <- paste("%." ,  est.dec, "f", sep="")
  ciformat <- paste("%." ,   ci.dec, "f", sep="")
  pvformat <- paste("%." , pval.dec, "f", sep="")
  
  ### function to format p-values to specified decimal places
  pvfun <- function(pvals){
    
    pvals2 <- sprintf(pvformat, round(pvals, pval.dec))
    
    if (pval.dec == 2) pvals2[pvals < 0.01  ]   <- "< 0.01"
    if (pval.dec == 3) pvals2[pvals < 0.001 ]   <- "< 0.001"
    if (pval.dec == 4) pvals2[pvals < 0.0001]   <- "< 0.0001"
    
    if (pval.dec == 2) pvals2[pvals > 0.99  ]   <- "> 0.99"
    if (pval.dec == 3) pvals2[pvals > 0.999 ]   <- "> 0.999"
    if (pval.dec == 4) pvals2[pvals > 0.9999]   <- "> 0.9999"
    
    if (htmlTable){
      pvals2 <- gsub("<", "&lt;", pvals2)
      pvals2 <- gsub(">", "&gt;", pvals2)
    }
    
    return(pvals2)
  }  
  
  ### function to format summary(glmfit) coef table
  tblfun <- function(fit){
    
    coef_tbl <- data.frame(summary(fit)$coef)
    
    ### remove rows with ordered intercepts: last nlevels - 1 rows of table
    out <- names(attr(terms(fit), "dataClasses")[1])
    nlevels_out <- nlevels(df[,out])
    coef_tbl <- coef_tbl[1:(nrow(coef_tbl)-nlevels_out+1),]
    
    method <- summary(fit)$method[1]
    
    ### calculate p-value
    coef_tbl$p_value <- pnorm(abs(coef_tbl[, "t.value"]), lower.tail = FALSE)*2
    
    if (is.na(exp) & method == "logistic") exp <- TRUE
    if (is.na(exp) & method != "logistic") exp <- TRUE
    
    ### if no estimate name is specified pick a reasonable name for each situation
    if (is.na(estname) & method == "logistic"      & exp == TRUE ) estname <- "OR"
    if (is.na(estname) & method == "logistic"      & exp == FALSE) estname <- "Estimate"
    if (is.na(estname) & method == "probit"                      ) estname <- "Estimate"
    if (is.na(estname) & method == "loglog"                      ) estname <- "Estimate"
    if (is.na(estname) & method == "cloglog"                     ) estname <- "Estimate"
    if (is.na(estname) & method == "cauchit"                     ) estname <- "Estimate"
    
    if ( exp) coef_tbl$Value <- exp(coef_tbl[,"Value"])
    coef_tbl$Value <- sprintf(esformat, round(coef_tbl$Value, est.dec))
    
    names(coef_tbl)[grepl("Value", names(coef_tbl))] <- estname
    
    ### conf function formats confidence intervals
    if (exp){
      conf <- function(x){
        paste("[",
              sprintf(ciformat, round(exp(x), ci.dec)[1]), ", ",
              sprintf(ciformat, round(exp(x), ci.dec)[2]) , "]", sep="")
      }
    }
    if (!exp){
      conf <- function(x){
        paste("[",
              sprintf(ciformat, round(x, ci.dec)[1]), ", ",
              sprintf(ciformat, round(x, ci.dec)[2]) , "]", sep="")
      }
    }
    
    cimat <- data.frame(confint(fit))
    if (nrow(coef_tbl) == 1) coef_tbl$CI <- conf(t(cimat))
    if (nrow(coef_tbl) >  1) coef_tbl$CI <- apply(cimat,1,conf)
    
    coef_tbl$p_value <- pvfun(pvals = coef_tbl$p_value)
    
    ###################
    
    ### get type II and type III p-values
    t2fit <- data.frame(Anova(fit, type = "II"))
    t3fit <- data.frame(Anova(fit, type = "III"))
    
    t2fit <- subset(t2fit, !is.na(Df))
    t3fit <- subset(t3fit, !is.na(Df))
    
    t2fit <- t2fit[!(grepl("frailty(*)", row.names(t2fit))),]
    t3fit <- t3fit[!(grepl("frailty(*)", row.names(t3fit))),]
    
    names(t2fit)[grepl("Pr" , names(t2fit))] <- "t2_pvalue"
    names(t3fit)[grepl("Pr" , names(t3fit))] <- "t3_pvalue"
    
    t2fit$t2_pvalue <- pvfun(pvals = t2fit$t2_pvalue)
    t3fit$t3_pvalue <- pvfun(pvals = t3fit$t3_pvalue)
    
    t2fit$varname <- row.names(t2fit)
    t3fit$varname <- row.names(t3fit)
    
    coef_tbl$coefname <- row.names(coef_tbl)
    
    vars <- NULL
    for (i in 1:nrow(t3fit)){
      vars <- c(vars, rep(t3fit[i, "varname"], t3fit[i,"Df"]))
    }
    
    ### get variable classes
    vtypes <- data.frame(attr(terms(fit), "dataClasses")[-1], stringsAsFactors = FALSE)
    names(vtypes) <- "vtype"
    vtypes$varname <- row.names(vtypes)
    
    ### get reference levels for factors
    xlevs <- fit$xlevels
    if (length(xlevs) > 0) {
      refs <- data.frame(sapply(xlevs, function(x) simcap(x[1])), stringsAsFactors = FALSE)
      names(refs) <- "ref"
      refs$varname <- row.names(refs)
    }
    if (length(xlevs) == 0){
      refs <- vtypes
      names(refs) <- c("ref", "varname")
      refs$ref <- NA
    }
    
    coef_tbl$varname <- vars
    
    coef_tbl$order <- 1:nrow(coef_tbl)
    
    coef_tbl2 <- merge(coef_tbl,   t2fit, by = "varname", all.x = TRUE, all.y = FALSE, sort = FALSE)
    coef_tbl2 <- merge(coef_tbl2,  t3fit, by = "varname", all.x = TRUE, all.y = FALSE, sort = FALSE)
    coef_tbl2 <- merge(coef_tbl2,   refs, by = "varname", all.x = TRUE, all.y = FALSE, sort = FALSE)
    coef_tbl2 <- merge(coef_tbl2, vtypes, by = "varname", all.x = TRUE, all.y = FALSE, sort = FALSE)
    
    coef_tbl2$vtype[grepl(":",coef_tbl2$coefname)] <- "interaction"
    
    ################
    
    coef_tbl2$levname <- NA
    for (i in 1:nrow(coef_tbl2)){
      coef_tbl2$levname[i] <- simcap(substring(coef_tbl2$coefname[i], nchar(coef_tbl2$varname[i]) + 1))
    }
    
    coef_tbl2$comp <- paste(coef_tbl2$levname, "vs.", coef_tbl2$ref)
    coef_tbl2$comp[!(coef_tbl2$vtype %in% c("character", "factor"))] <- NA
    
    coef_tbl2 <- coef_tbl2[order(coef_tbl2$order), ]
    
    coef_tbl2$vseq <- ave(coef_tbl2$order, coef_tbl2$varname, FUN = function(x) seq_along(x))
    
    # create a duplicate row for header - just for factors
    coef_tbl2 <- coef_tbl2[rep(seq_len(nrow(coef_tbl2)), each=2),]
    coef_tbl2$dup <- 0
    coef_tbl2$dup[grepl(".1", row.names(coef_tbl2), fixed = T)] <- 1
    coef_tbl2 <- subset(coef_tbl2, dup == 0 | (vtype %in% c("character", "factor", "interaction") & vseq == 1))
    
    coef_tbl2 <- coef_tbl2[order(coef_tbl2$order, -coef_tbl2$dup),]
    
    coef_tbl2$compref <- coef_tbl2$levname
    coef_tbl2$compref[coef_tbl2$dup == 1] <- paste(as.character(coef_tbl2$ref[coef_tbl2$dup == 1]), "(Ref)")
    coef_tbl2$compref[!(coef_tbl2$vtype %in% c("factor", "character"))] <- NA
    
    coef_tbl2$comp[coef_tbl2$dup == 1] <- NA
    
    coef_tbl2[coef_tbl2$dup == 1, estname  ] <- NA
    coef_tbl2[coef_tbl2$dup == 1, "CI"     ] <- NA
    coef_tbl2[coef_tbl2$dup == 1, "p_value"] <- NA
    coef_tbl2[coef_tbl2$dup == 0 & !(coef_tbl2$vtype %in% c("numeric","integer")), "t2_pvalue"] <- NA
    coef_tbl2[coef_tbl2$dup == 0 & !(coef_tbl2$vtype %in% c("numeric","integer")), "t3_pvalue"] <- NA
    
    coef_tbl2$order <- 1:nrow(coef_tbl2)
    coef_tbl2$vseq <- ave(coef_tbl2$order, coef_tbl2$varname, FUN = function(x) seq_along(x))
    coef_tbl2$vrows <- ave(coef_tbl2$order, coef_tbl2$varname, FUN = function(x) length(x))
    coef_tbl2$varname[coef_tbl2$vseq > 1] <- NA
    
    coef_tbl2$comp[coef_tbl2$dup == 0 & coef_tbl2$vtype == "interaction"] <- coef_tbl2$coefname[coef_tbl2$dup == 0 & coef_tbl2$vtype == "interaction"]
    coef_tbl2$compref[coef_tbl2$dup == 0 & coef_tbl2$vtype == "interaction"] <- coef_tbl2$coefname[coef_tbl2$dup == 0 & coef_tbl2$vtype == "interaction"]
    
    if ( refcat) coef_tbl3 <- coef_tbl2[,c("varname", "compref", estname, "CI", "p_value", "t2_pvalue", "t3_pvalue")]
    if (!refcat) coef_tbl3 <- coef_tbl2[,c("varname", "comp"   , estname, "CI", "p_value", "t2_pvalue", "t3_pvalue")]
    
    vrows  <- coef_tbl2$vrows[coef_tbl2$vseq == 1]
    vnames <- coef_tbl3$varname[coef_tbl2$vseq == 1]
    
    return(list("tbl"     = coef_tbl3
                ,"vrows"  = vrows
                ,"vnames" = vnames
                ,"estname" = estname
          ))
  }
  
  # Create multiple regression table ----------------------------------------
  
  if (regtype == "multi") {
    
    if (is.na(fit[1]) & sum(!is.na(covs) > 0) & !is.na(out[1])){
      
      form <- as.formula(paste(out, "~", paste(covs, collapse = "+")))
      
      polrfit <- polr(form
                     ,data = df
                     ,method=method
                     ,Hess = TRUE
                     )
      fit <- polrfit
    }
    
    tbl <- tblfun(fit)
    
    ftbl <- tbl[["tbl"]]
    ftbl <- ftbl[,names(ftbl) != "varname"]
    
    estname = tbl[["estname"]]
    
    head <- c("Variable", estname, "95% CI", "Wald p-value", "Type II p", "Type III p")
    alignr <- "lccrrr"
    
    if (type2 & !type3) {
      ftbl <- ftbl[,names(ftbl) != "t3_pvalue"]
      head <- c("Variable", estname, "95% CI", "Wald p-value", "Type II p")
      alignr <- "lccrr"
    }
    if (!type2 & type3) {
      ftbl <- ftbl[,names(ftbl) != "t2_pvalue"]
      head <- c("Variable", estname, "95% CI", "Wald p-value", "Type III p")
      alignr <- "lccrr"
    }
    if (!type2 & !type3) {
      ftbl <- ftbl[,!(names(ftbl) %in% c("t2_pvalue", "t3_pvalue"))]
      head <- c("Variable", estname, "95% CI", "p-value")
      alignr <- "lccr"
    }
    
    vnames = tbl[["vnames"]]
    if (!is.na(labels[1]) & sum(!is.na(labels)) == length(vnames)){
      vnames <- labels
    }
    
    footnote <- paste("Prop. Odds Logistic Regression, N = ", fit$n, ")", sep= "")

    if (!footer) footnote <- ""
    
    if (htmlTable & kable) htmlTable <- FALSE
    
    if (!htmlTable & !kable){
      return(ftbl)
    }
    if (htmlTable){
      print(
        htmlTable(ftbl,
                  header = head,
                  caption = title,
                  tfoot = footnote,
                  rnames = FALSE,
                  align = alignr,
                  rgroup = vnames,
                  n.rgroup = tbl[["vrows"]],
                  col.rgroup = c(color, "white"),
                  css.cell = "padding-left: 2em; padding-right: 1em;")
      )
    }
    if (kable){
      
      ### get header rows
      headr <- which(is.na(ftbl[,1]))
      ### get non-header rows
      nonheadr <- which(!is.na(ftbl[,1]))
      ### indent non-header rows and remove *
      ftbl[nonheadr,1] <- paste("&nbsp; &nbsp; &nbsp;", ftbl[nonheadr,1])
      
      ### bold header rows
      ftbl[headr,1] <- vnames
      ftbl[headr,1] <- paste("<b>", ftbl[headr,1], "<b/>", sep="")
      
      names(ftbl) <- head
      
      if (title == "" & footer) title <- footnote
      
      print(
        kable(x = ftbl,
              row.names = FALSE,
              caption = title,
              align = alignr)
      )
    }
  }
  
  # Create univariate regression table --------------------------------------
  
  if (regtype == "uni") {
    
    tbl_uni <- NULL
    vrows_uni <- NULL
    vnames_uni <- NULL
    nobs_uni <- NULL
    
    for (j in 1:length(covs)){
      
      ### if model is not provided, run the model
  
      if (is.na(fit[1]) & sum(!is.na(covs) > 0)  & !is.na(out[1])){
        formj <- as.formula(paste(out, "~", covs[j]))
        
        polrfitj <- polr(formj
                        ,data = df
                        ,method=method
                        ,Hess = TRUE
        )
        fitj <- polrfitj
      }
      
      tblj <- tblfun(fitj)
      
      vnamej = tblj[["vnames"]]
      if (!is.na(labels[1]) & sum(!is.na(labels)) == length(covs)){
        vnamej <- labels[j]
      }
      
      tbl_uni    <- rbind(tbl_uni   , tblj[["tbl"]])
      vrows_uni  <- c(vrows_uni , tblj[["vrows"]])
      vnames_uni <- c(vnames_uni, vnamej)
      nobs_uni <- c(nobs_uni, paste(" (N = ", fitj$n, ")", sep=""))
    }
    
    ftbl <- tbl_uni
    ftbl <- ftbl[,names(ftbl) != "varname"]
    
    estname <- tblj[["estname"]]
    
    head <- c("Variable", estname, "95% CI", "Wald p-value", "Type II p", "Type III p")
    alignr <- "lccrrr"
    
    if (type2 & !type3) {
      ftbl <- ftbl[,names(ftbl) != "t3_pvalue"]
      head <- c("Variable", estname, "95% CI", "Wald p-value", "Type II p")
      alignr <- "lccrr"
    }
    if (!type2 & type3) {
      ftbl <- ftbl[,names(ftbl) != "t2_pvalue"]
      head <- c("Variable", estname, "95% CI", "Wald p-value", "Type III p")
      alignr <- "lccrr"
    }
    if (!type2 & !type3) {
      ftbl <- ftbl[,!(names(ftbl) %in% c("t2_pvalue", "t3_pvalue"))]
      head <- c("Variable", estname, "95% CI", "p-value")
      alignr <- "lccr"
    }
    
    footnote <- "Univariate (unadjusted) Prop. Odds Logisitic Regression"
    if (!footer) footnote <- ""
    
    vnames_uni <- paste(vnames_uni, nobs_uni)
    
    if (htmlTable & kable) htmlTable <- FALSE
    
    if (!htmlTable & !kable){
      return(ftbl)
    }
    if (htmlTable){
      
      print(
        htmlTable(ftbl,
                  header = head,
                  caption = title,
                  tfoot = footnote,
                  rnames = FALSE,
                  align = alignr,
                  rgroup = vnames_uni,
                  n.rgroup = vrows_uni,
                  col.rgroup = c(color, "white"),
                  css.cell = "padding-left: 3em; padding-right: 1em;")
      )
    }
    if (kable){
      
      ### get header rows
      headr <- which(is.na(ftbl[,1]))
      ### get non-header rows
      nonheadr <- which(!is.na(ftbl[,1]))
      ### indent non-header rows and remove *
      ftbl[nonheadr,1] <- paste("&nbsp; &nbsp; &nbsp;", ftbl[nonheadr,1])
      
      ### bold header rows
      ftbl[headr,1] <- vnames_uni
      ftbl[headr,1] <- paste("<b>", ftbl[headr,1], "<b/>", sep="")
      
      names(ftbl) <- head
      
      if (title == "" & footer) title <- footnote
      
      print(
        kable(x = ftbl,
              row.names = FALSE,
              caption = title,
              align = alignr)
      )
    }
    
  }
  
  ### restore contrast settings
  options(contrasts = unlist(current_contr))
}
 





