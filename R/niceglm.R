#' Lisa's GLM Regression Table Reporting Function 
#'
#' This function creates a nice looking table of regression results for a glm model. 
#' Input either a glm object or dataframe, outcome variable, vector of covariates, model family, and type of analysis (univariate or multiple regression). 
#' The function returns a dataframe of formatted regression results and prints the results table using htmlTable.
#' @param fit GLM model object [fit or family/covs/out are REQUIRED].
#' @param df Dataframe [fit or df/family/covs/out]. 
#' @param family Character. Model family name in quotes ("guassian", "binomial", "poisson") [fit or df/family/covs/out are REQUIRED]. 
#' @param covs Character. Vector of covariates to include in model [fit or df/family/covs/out are REQUIRED].
#' @param out Character. Outcome for regression model [fit or df/family/covs/out are REQUIRED].
#' @param regtype Logical. Should the covariates be run separately ("uni") or together in a multiple regression model ("multi") [REQUIRED if no fit].  
#' @param intercept Logical. If TRUE the intercept will be included in the table (muliple regression only). Default is FALSE.
#' @param refcat Logical. If TRUE the table will create a separate line for the reference category. Default is FALSE.
#' @param overallp Logical. If TRUE, a likelihood ratio test pvalue will be calculated for each variable (via drop1, Chisq test). Default is FALSE.
#' @param est.dec Numeric. Number of decimal places for estimates. Default is 2.
#' @param ci.dec Numeric. Number of decimal places for 95% confidence interval. Default is 2.
#' @param pval.dec Numeric. Number of decimal places for pvalues. Must be an integer from 1 to 4. Default is 3.
#' @param estname Character. Label for regression estimate. Default is NA (program will choose a reasonable name depending on model type).
#' @param exp Logical. Option to exponentiate coefficients and CI's. Default is NA (estimates exponentiated for binomial and poisson models by default).
#' @param color Character. Color to use for htmlTable striping. Default is "#EEEEEE" (light grey). Use "white" for no striping.
#' @param htmlTable Logical. If TRUE, the table will be optimized for html output. Default is TRUE.
#' @param title Character. Optional title above table. Default is "".
#' @param footer Logical. If TRUE, table will include a footnote with model details, nobs, R2. Default is TRUE.
#' @keywords glm table logistic poisson linear regression reporting
#' @importFrom htmlTable htmlTable
#' @export 
niceglm    <- function(fit = NA,
                       df = NA, 
                       family = NA,
                       covs = NA,
                       out = NA,
                       regtype = "multi",
                       exp = NA,
                       estname = NA,
                       intercept = FALSE,
                       refcat = FALSE,
                       labels = NA, 
                       overallp = FALSE,
                       est.dec = 2,
                       ci.dec = 2,
                       pval.dec = 3,
                       color = "#EEEEEE",
                       htmlTable = TRUE,
                       title = "",
                       footer = TRUE){
    
    
    # check user inputs -------------------------------------------------------
    
    if (!is.na(fit[1])) regtype <- "multi"
    try(if (is.na(fit[1]) & (is.na(regtype) | !(regtype %in% c("uni", "multi")))) stop("regtype must be uni or multi\n"))
    
    if (is.na(fit[1])){
        
        try(if (class(df) != "data.frame" | is.na(covs[1]) | is.na(out[1]) | is.na(family[1])) stop("must provide model object or dataframe + covariates + outcome + model family\n"))
        
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
        try(if (class(family[1]) != "character" | !family %in% c("gaussian", "binomial", "Gamma", "poisson", "quasibinomial", "quasipoisson")) stop ("invalid family, must be: gaussian, binomial, Gamma, poisson, quasibinomial, quasipoisson\n"))
    }
    
    ### do not include intercepts for univariate tables
    if (regtype == "uni") intercept = FALSE
    
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
        
        family <- summary(fit)$family["family"]
        
        exp <- ifelse(is.na(exp) & family == "gaussian", FALSE, TRUE)
        
        ### if no estimate name is specified pick a reasonable name for each situation
        if (!is.na(fit[1])) family <- summary(fit)$family[1]
        if (is.na(estname) & family == "binomial"      & exp == TRUE ) estname <- "OR"
        if (is.na(estname) & family == "poisson"       & exp == TRUE ) estname <- "RR"
        if (is.na(estname) & family == "quasibinomial" & exp == TRUE ) estname <- "OR"
        if (is.na(estname) & family == "quasipoisson"  & exp == TRUE ) estname <- "RR"
        if (is.na(estname) & family == "gaussian"      & exp == TRUE ) estname <- "Ratio"
        if (is.na(estname) & family == "quasibinomial" & exp == FALSE) estname <- "Estimate"
        if (is.na(estname) & family == "quasipoisson"  & exp == FALSE) estname <- "Estimate"
        if (is.na(estname) & family == "binomial"      & exp == FALSE) estname <- "Estimate"
        if (is.na(estname) & family == "poisson"       & exp == FALSE) estname <- "Estimate"
        if (is.na(estname) & family == "gaussian"      & exp == FALSE) estname <- "Difference"
        
        if ( exp) coef_tbl$Estimate <- exp(summary(fit)$coef[,"Estimate"])
        if (!exp) coef_tbl$Estimate <- summary(fit)$coef[,"Estimate"]
        coef_tbl$Estimate <- sprintf(esformat, round(coef_tbl$Estimate, est.dec))
        
        names(coef_tbl)[grepl("Est", names(coef_tbl))] <- estname
        names(coef_tbl)[grepl("Pr" , names(coef_tbl))] <- "p_value"
        
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
        
        # coef_tbl <- coef_tbl[,c(estname, "CI", "p_value")]
        
        if (!overallp) {
            dr1fit <- data.frame(drop1(fit, scope = attr(terms(fit), "term.labels")))
            dr1fit$op_value <- NA
        }
        if ( overallp) {
            dr1fit <- data.frame(drop1(fit, scope = attr(terms(fit), "term.labels"), test = "Chisq"))
            names(dr1fit)[grepl("Pr" , names(dr1fit))] <- "op_value"
            dr1fit$op_value <- pvfun(pvals = dr1fit$op_value)
        }
        
        row.names(dr1fit)[1] <- "(Intercept)"
        dr1fit$Df[1] <- 1
        
        dr1fit$varname <- row.names(dr1fit)
        
        coef_tbl$coefname <- row.names(coef_tbl)
        
        vars <- NULL
        for (i in 1:nrow(dr1fit)){
            vars <- c(vars, rep(dr1fit[i, "varname"], dr1fit[i,"Df"]))
        }
        
        ### get variable classes
        vtypes <- data.frame(attr(terms(fit), "dataClasses")[-1])
        names(vtypes) <- "vtype"
        vtypes$varname <- row.names(vtypes)
        
        ### get reference levels for factors
        
        xlevs <- fit$xlevels
        if (length(xlevs) > 0) {
            refs <- data.frame(sapply(xlevs, function(x) simcap(x[1])))
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
        
        coef_tbl2 <- merge(coef_tbl,  dr1fit, by = "varname", all.x = TRUE, all.y = FALSE, sort = FALSE)
        coef_tbl2 <- merge(coef_tbl2,   refs, by = "varname", all.x = TRUE, all.y = FALSE, sort = FALSE)
        coef_tbl2 <- merge(coef_tbl2, vtypes, by = "varname", all.x = TRUE, all.y = FALSE, sort = FALSE)
        coef_tbl2$nobs <- nobs(fit)
        
        coef_tbl2$levname <- NA
        for (i in 1:nrow(coef_tbl2)){
            coef_tbl2$levname[i] <- simcap(substring(coef_tbl2$coefname[i], nchar(coef_tbl2$varname[i]) + 1))
        }
        
        coef_tbl2$comp <- paste(coef_tbl2$levname, "vs.", coef_tbl2$ref)
        coef_tbl2$comp[!(coef_tbl2$vtype %in% c("character", "factor"))] <- NA
        
        # coef_tbl2$varname <- sapply(coef_tbl2$varname, function(x) simcap(x))
        
        coef_tbl2 <- coef_tbl2[order(coef_tbl2$order), ]
        
        coef_tbl2$vseq <- ave(coef_tbl2$order, coef_tbl2$varname, FUN = function(x) seq_along(x))
        
        # create a duplicate row for header - just for factors 
        coef_tbl2 <- coef_tbl2[rep(seq_len(nrow(coef_tbl2)), each=2),]
        coef_tbl2$dup <- 0
        coef_tbl2$dup[grepl(".1", row.names(coef_tbl2))] <- 1
        coef_tbl2 <- subset(coef_tbl2, dup == 0 | (vtype %in% c("character", "factor") & vseq == 1))
        
        coef_tbl2 <- coef_tbl2[order(coef_tbl2$order, -coef_tbl2$dup),]
        
        if (!intercept | regtype == "uni") coef_tbl2 <- subset(coef_tbl2, varname != "(Intercept)")
        
        coef_tbl2$compref <- coef_tbl2$levname
        coef_tbl2$compref[coef_tbl2$dup == 1] <- paste(as.character(coef_tbl2$ref[coef_tbl2$dup == 1]), "(Ref)")
        coef_tbl2$compref[!(coef_tbl2$vtype %in% c("factor", "character"))] <- NA
        
        coef_tbl2$comp[coef_tbl2$dup == 1] <- NA
        
        coef_tbl2[coef_tbl2$dup == 1, estname  ] <- NA
        coef_tbl2[coef_tbl2$dup == 1, "CI"     ] <- NA
        coef_tbl2[coef_tbl2$dup == 1, "p_value"] <- NA
        coef_tbl2[coef_tbl2$dup == 0 & coef_tbl2$vtype %in% c("character", "factor"), "op_value"] <- NA
        
        coef_tbl2$order <- 1:nrow(coef_tbl2)
        coef_tbl2$vseq <- ave(coef_tbl2$order, coef_tbl2$varname, FUN = function(x) seq_along(x))
        coef_tbl2$vrows <- ave(coef_tbl2$order, coef_tbl2$varname, FUN = function(x) length(x))
        coef_tbl2$varname[coef_tbl2$vseq > 1] <- NA
        
        if ( refcat) coef_tbl3 <- coef_tbl2[,c("varname", "compref", estname, "CI", "p_value", "op_value")]
        if (!refcat) coef_tbl3 <- coef_tbl2[,c("varname", "comp"   , estname, "CI", "p_value", "op_value")]
        
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
        
        ### if model is not provided, run the model
        if (is.na(fit[1]) & sum(!is.na(covs) > 0)  & !is.na(out) & !is.na(family)){
            form <- as.formula(paste(out, "~", paste(covs, collapse = " + ")))
            fit <- glm(form, data = df, family = family)
        }
        
        tbl <- tblfun(fit)
        
        ftbl <- tbl[["tbl"]]
        ftbl <- ftbl[,names(ftbl) != "varname"]
        
        head <- c("Variable", estname, "95% CI", "Wald p-value", "LR p-value")
        alignr <- "llccrr"
        
        if (!overallp) {
            ftbl <- ftbl[,names(ftbl) != "op_value"]
            head <- c("Variable", estname, "95% CI", "p-value")
            alignr <- "llccr"
        }
        
        vnames = tbl[["vnames"]]
        if (!is.na(labels[1]) & sum(!is.na(labels)) == length(vnames)){
            vnames <- labels
        }
        
        footnote <- paste("Family = ", simcap(as.character(fit$family[1])),
                        ", ",
                        "Link = ", simcap(as.character(fit$family[2])),
                        ", ",
                        "N obs = ", nobs(fit),
                        ", ",
                        "R2 = ", sprintf("%.3f", round(1-(fit$deviance/fit$null.deviance), 3)),
                        sep= "")
        if (htmlTable) footnote <- gsub("R2", "R<sup>2</sup>", footnote)
        if (!footer) footnote <- ""
        
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
                      css.cell = "padding-left: 3em; padding-right: 1em;")
        )
        
    }
    
    # Create univariate regression table --------------------------------------
    
    if (regtype == "uni") {
        
        tbl_uni <- NULL
        vrows_uni <- NULL
        vnames_uni <- NULL
        nobs_uni <- NULL
        
        for (j in 1:length(covs)){
            
            formj <- as.formula(paste(out, "~", covs[j]))
            fitj <- glm(formj, data = df, family = family)
            
            tblj <- tblfun(fitj)
            
            tbl_uni    <- rbind(tbl_uni   , tblj[["tbl"]])
            vrows_uni  <- c(vrows_uni , tblj[["vrows"]])
            vnames_uni <- c(vnames_uni, tblj[["vnames"]])
            nobs_uni <- c(nobs_uni, paste("(N = ", nobs(fitj), ")", sep=""))
            
        }
        
        ftbl <- tbl_uni
        ftbl <- ftbl[,names(ftbl) != "varname"]
        
        estname <- tblj[["estname"]]
        
        head <- c("Variable", estname, "95% CI", "Wald p-value", "LR p-value")
        alignr <- "llccrr"
        
        if (!overallp) {
            ftbl <- ftbl[,names(ftbl) != "op_value"]
            head <- c("Variable", estname, "95% CI", "p-value")
            alignr <- "llccr"
        }
        
        if (!is.na(labels[1]) & sum(!is.na(labels)) == length(vnames_uni)){
            vnames_uni <- labels
        }
        
        footnote <- paste("Family = ", simcap(as.character(fitj$family[1])),
                          ", ",
                          "Link = ", simcap(as.character(fitj$family[2])),
                          sep= "")
        if (!footer) footnote <- ""
        
        vnames_uni <- paste(vnames_uni, nobs_uni)
        
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
    
    return(ftbl)
}