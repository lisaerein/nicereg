#' Lisa's GLMER Regression Table Reporting Function
#'
#' This function creates a nice looking table of regression results for a glmer (lme4 package) model.
#' Input either a glmer object or dataframe, outcome variable, vector of covariates, random effects statement, model family, and type of analysis (univariate or multiple regression).
#' The function returns a dataframe of formatted regression results and prints the results table using kable or htmlTable.
#' @param fit glmer model object [fit or family/covs/out/random are REQUIRED].
#' @param df Dataframe [fit or family/covs/out/random REQUIRED].
#' @param family Character. Model family name in quotes ("guassian", "binomial", "poisson") [fit or family/covs/out/random are REQUIRED].
#' @param covs Character. Vector of covariates to include in model [fit or family/covs/out/random are REQUIRED].
#' @param out Character. Outcome for regression model [fit or family/covs/out/random are REQUIRED].
#' @param random Character. Random effects statement to add to formula, ex. "(1|study_id)" for random intercept by study_id. [fit or family/covs/out/random are REQUIRED].
#' @param optimizer Character. Option to pass to glmerControl. Default is "bobyqa".
#' @param regtype Logical. Should the covariates be run separately ("uni") or together in a multiple regression model ("multi") [fit or family/covs/out/id/corstr are REQUIRED].
#' @param intercept Logical. If TRUE the intercept will be included in the table (muliple regression only). Default is FALSE.
#' @param refcat Logical. If TRUE the table will create a separate line for the reference category. Default is FALSE.
#' @param labels Character vector. Covariate labels in same order as covs. Default is NA (variable names are used).
#' @param est.dec Numeric. Number of decimal places for estimates. Default is 2.
#' @param ci.dec Numeric. Number of decimal places for 95 percent confidence interval. Default is 2.
#' @param pval.dec Numeric. Number of decimal places for pvalues. Must be an integer from 1 to 4. Default is 3.
#' @param estname Character. Label for regression estimate. Default is NA (program will choose a reasonable name depending on model type).
#' @param exp Logical. Option to exponentiate coefficients and CI's. Default is NA (estimates exponentiated for binomial and poisson models by default).
#' @param color Character. Color to use for htmlTable striping. Default is "#EEEEEE" (light grey). Use "white" for no striping.
#' @param kable Logical. If TRUE, table will be formatted using the kable packge. Default is TRUE.
#' @param htmlTable Logical. If TRUE, the table will be printed using htmlTable. Default is FALSE.
#' @param title Character. Optional title above table. Default is "".
#' @param footer Logical. If TRUE, table will include a footnote with model details, nobs, R2. Default is TRUE.
#' @keywords glmer mixed random effects table logistic poisson linear regression reporting nice lisaerein
#' @importFrom lme4 glmer lmer glmerControl lmerControl
#' @importFrom knitr kable
#' @importFrom htmlTable htmlTable
#' @export
niceglmer <- function(fit = NA
                     ,df = NA
                     ,family = NA
                     ,covs = NA
                     ,out = NA
                     ,random = NA
                     ,optimizer = "bobyqa"
                     ,regtype = "multi"
                     ,exp = NA
                     ,estname = NA
                     ,intercept = FALSE
                     ,refcat = FALSE
                     ,labels = NA
                     ,est.dec = 2
                     ,ci.dec = 2
                     ,pval.dec = 3
                     ,color = "#EEEEEE"
                     ,kable = TRUE
                     ,htmlTable = FALSE
                     ,title = ""
                     ,footer = TRUE
                      ){

      # check user inputs -------------------------------------------------------

      overallp = FALSE

      if (!is.null(attributes(class(fit))$package)) regtype <- "multi"

      try(if (is.null(attributes(class(fit))$package) & (is.na(regtype) | !(regtype %in% c("uni", "multi")))) stop("regtype must be uni or multi\n"))

      if (is.null(attributes(class(fit))$package)){

            try(if (class(df) != "data.frame" | is.na(covs[1]) | is.na(out[1]) | is.na(family[1])) stop("must provide model object or dataframe + covariates + outcome + model family\n"))

            ## remove any covs that do not appear in dataset
            covs2 <- covs[covs %in% names(df)]
            if (length(covs2) != length(covs)) cat("Warning! Covariate(s) do not exist in dataset:", covs[!(covs %in% names(df))],"\n")
            covs <- covs2
            try(if (length(covs) == 0) stop("No valid covs\n"))

            ## check that outcome appears in dataset
            out2 <- out[out %in% names(df)]
            try(if (length(out2) != 1) stop("Outcome does not exist in dataset: ", out[!(out %in% names(df))],"\n"))
            out <- out2

            ## check that random variable(s) appears in dataset
            if (!is.na(random)){
                  randvars <- gsub(" ", "", random)
                  randvars <- gsub("\\(|\\)|\\+", "-", randvars)
                  randvars <- gsub("\\|", "-", randvars)
                  randvars <- strsplit(randvars, "-")[[1]]
                  randvars <- randvars[!(randvars %in% c("", "1"))]
                  randvars2 <- randvars[randvars %in% names(df)]
                  try(if (length(randvars2) != length(randvars)) stop("Random variable(s) do not exist in dataset: ", randvars[!(randvars %in% names(df))],"\n"))
            }

            try(if (class(covs[1]) != "character") stop("covs must be a character vector\n"))
            try(if (class(out[1]) != "character" | length(out) != 1) stop("out must be a single character string\n"))
            try(if (class(family[1]) != "character" | !family %in% c("gaussian", "binomial", "poisson")) stop ("invalid family, must be: gaussian, binomial, poisson\n"))

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

      ### function to format summary(geeglm) coef table
      tblfun <- function(fit){

            coef_tbl <- data.frame(summary(fit)$coef, stringsAsFactors = FALSE)

            family <- summary(fit)$family

            exp <- ifelse(is.na(exp) & family == "gaussian", FALSE, TRUE)

            ncovs <- ncol(fit@frame) - 1 - length(names(fit@flist))

            ### if no estimate name is specified pick a reasonable name for each situation
            if (is.na(estname) & family == "binomial"  &  exp & ncovs == 1) estname <- "OR"
            if (is.na(estname) & family == "poisson"   &  exp & ncovs == 1) estname <- "RR"
            if (is.na(estname) & family == "gaussian"  &  exp & ncovs == 1) estname <- "Fold Change"
            if (is.na(estname) & family == "binomial"  & !exp & ncovs == 1) estname <- "Estimate"
            if (is.na(estname) & family == "poisson"   & !exp & ncovs == 1) estname <- "Estimate"
            if (is.na(estname) & family == "gaussian"  & !exp & ncovs == 1) estname <- "Difference"

            if (is.na(estname) & family == "binomial"  &  exp & ncovs > 1) estname <- "aOR"
            if (is.na(estname) & family == "poisson"   &  exp & ncovs > 1) estname <- "aRR"
            if (is.na(estname) & family == "gaussian"  &  exp & ncovs > 1) estname <- "aFCh"
            if (is.na(estname) & family == "binomial"  & !exp & ncovs > 1) estname <- "Adj Estimate"
            if (is.na(estname) & family == "poisson"   & !exp & ncovs > 1) estname <- "Adj Estimate"
            if (is.na(estname) & family == "gaussian"  & !exp & ncovs > 1) estname <- "aDiff"

            coef_tbl$lower <- coef_tbl[,"Estimate"] - (qnorm(0.975)*coef_tbl[,"Std..Error"])
            coef_tbl$upper <- coef_tbl[,"Estimate"] + (qnorm(0.975)*coef_tbl[,"Std..Error"])

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

            cimat <- data.frame(coef_tbl[,c("lower", "upper")])
            if (nrow(coef_tbl) == 1) coef_tbl$CI <- conf(t(cimat))
            if (nrow(coef_tbl) >  1) coef_tbl$CI <- apply(cimat,1,conf)

            coef_tbl$p_value <- pvfun(pvals = coef_tbl$p_value)

            ### get list of variable names without effect attached
            dr1fit <- data.frame(anova(fit), stringsAsFactors = FALSE)
            varnames <- rownames(dr1fit)

            dr1fit$varname <- trim(row.names(dr1fit))
            dr1fit <- dr1fit[dr1fit$varname != "Residuals",]

            blank <- dr1fit[1,]
            blank[1,] <- rep(NA, ncol(blank))
            row.names(blank) <- "(Intercept)"
            blank$Df <- 1
            dr1fit <- rbind(blank, dr1fit)

            coef_tbl$coefname <- row.names(coef_tbl)

            vars <- NULL
            for (i in 1:nrow(dr1fit)){
                  vars <- c(vars, rep(dr1fit[i, "varname"], dr1fit[i,"Df"]))
            }

            ### create lookup dataset for variable classes
            vtype <- rep(NA, length(varnames))
            for (i in 1:length(varnames)) vtype[i] <- class(fit@frame[,varnames[i]])
            vtypes <- as.data.frame(vtype, drop = F, stringsAsFactors = FALSE)
            vtypes$varname <- varnames

            ### create lookup dataset of reference levels for factor variables
            ref <- rep(NA, length(varnames))
            for (i in 1:length(varnames)){
                  if (vtype[i] %in% c("character", "factor")) ref[i] <- levels(fit@frame[,varnames[i]])[1]
            }
            refs <- as.data.frame(ref, drop = F, stringsAsFactors = FALSE)
            refs$varname <- varnames

            coef_tbl$varname2 <- vars
            coef_tbl$varname <- coef_tbl$varname2
            coef_tbl$varname[grepl("Intercept", coef_tbl$coefname)] <- "(Intercept)"

            coef_tbl$order <- 1:nrow(coef_tbl)

            coef_tbl$op_value <- NA
            coef_tbl2 <- merge(coef_tbl ,   refs, by = "varname", all.x = TRUE, all.y = FALSE, sort = FALSE)
            coef_tbl2 <- merge(coef_tbl2, vtypes, by = "varname", all.x = TRUE, all.y = FALSE, sort = FALSE)

            coef_tbl2$vtype[grepl(":",coef_tbl2$coefname)] <- "interaction"

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
            coef_tbl2$dup[grepl(".1", row.names(coef_tbl2))] <- 1
            coef_tbl2 <- subset(coef_tbl2, dup == 0 | (vtype %in% c("character", "factor", "interaction") & vseq == 1))

            coef_tbl2 <- coef_tbl2[order(coef_tbl2$order, -coef_tbl2$dup),]

            if (!intercept | regtype == "uni") coef_tbl2 <- subset(coef_tbl2, varname != "(Intercept)")

            coef_tbl2$compref <- coef_tbl2$levname
            coef_tbl2$compref[coef_tbl2$dup == 1] <- paste(as.character(coef_tbl2$ref[coef_tbl2$dup == 1]), "(Ref)")
            coef_tbl2$compref[!(coef_tbl2$vtype %in% c("factor", "character"))] <- NA

            coef_tbl2$comp[coef_tbl2$dup == 1] <- NA

            coef_tbl2[coef_tbl2$dup == 1, estname  ] <- NA
            coef_tbl2[coef_tbl2$dup == 1, "CI"     ] <- NA
            coef_tbl2[coef_tbl2$dup == 1, "p_value"] <- NA
            coef_tbl2[coef_tbl2$dup == 0 & !(coef_tbl2$vtype %in% c("numeric", "integer")), "op_value"] <- NA

            coef_tbl2$order <- 1:nrow(coef_tbl2)
            coef_tbl2$vseq <- ave(coef_tbl2$order, coef_tbl2$varname, FUN = function(x) seq_along(x))
            coef_tbl2$vrows <- ave(coef_tbl2$order, coef_tbl2$varname, FUN = function(x) length(x))
            coef_tbl2$varname[coef_tbl2$vseq > 1] <- NA

            coef_tbl2$comp[coef_tbl2$dup == 0 & coef_tbl2$vtype == "interaction"] <- coef_tbl2$coefname[coef_tbl2$dup == 0 & coef_tbl2$vtype == "interaction"]
            coef_tbl2$compref[coef_tbl2$dup == 0 & coef_tbl2$vtype == "interaction"] <- coef_tbl2$coefname[coef_tbl2$dup == 0 & coef_tbl2$vtype == "interaction"]

            if ( refcat) coef_tbl3 <- coef_tbl2[,c("varname", "compref", estname, "CI", "p_value", "op_value")]
            if (!refcat) coef_tbl3 <- coef_tbl2[,c("varname", "comp"   , estname, "CI", "p_value", "op_value")]

            vrows  <- coef_tbl2$vrows[coef_tbl2$vseq == 1]
            vnames <- coef_tbl3$varname[coef_tbl2$vseq == 1]

            return(list("tbl"     = coef_tbl3
                       ,"vrows"   = vrows
                       ,"vnames"  = vnames
                       ,"estname" = estname
                       )
                   )
      }

      # Create multiple regression table ----------------------------------------

      if (regtype == "multi") {

            ### if model is not provided, run the model
            if (is.null(attributes(class(fit))$package) & sum(!is.na(covs) > 0)  & !is.na(out) & !is.na(family) & !is.na(random)){

                  dfc <- df[complete.cases(df[,c(out, covs, randvars)]),]

                  for (r in 1:length(randvars)) dfc[,randvars[r]] <- factor(dfc[,randvars[r]])

                  for (q in 1:length(covs)){
                        if ("factor" %in% class(dfc[,covs[q]])) dfc[,covs[q]] <- droplevels(dfc[,covs[q]])
                  }

                  form <- as.formula(paste(paste(out, "~", paste(covs, collapse = " + ")),
                                           "+",
                                           random))
                  fit <- glmer(form
                              ,data = dfc
                              ,family = family
                              ,control = glmerControl(optimizer = optimizer)
                              )
            }

            tbl <- tblfun(fit)

            ftbl <- tbl[["tbl"]]
            ftbl <- ftbl[,names(ftbl) != "varname"]

            if (is.na(estname)) estname <- tbl[["estname"]]

            head <- c("Variable", estname, "95% CI", "Wald p-value", "LR p-value")
            alignr <- "llccrr"

            ftbl <- ftbl[,names(ftbl) != "op_value"]
            head <- c("Variable", estname, "95% CI", "p-value")
            alignr <- "llccr"

            vnames = tbl[["vnames"]]
            if (!is.na(labels[1]) & sum(!is.na(labels)) == length(vnames)){
                  vnames <- labels
            }

            footnote <- paste("Family = ", simcap(as.character(summary(fit)$family)),
                              ", ",
                              "N obs = ", nobs(fit),
                              sep= "")
            if (!is.na(random)){
                  footnote <- paste("Mixed effecs regression, Family = "
                                   ,simcap(as.character(summary(fit)$family))
                                   ,", "
                                   ,"N obs = ", nobs(fit)
                                   ,sep= ""
                                   )
            }

            if (!footer) footnote <- ""
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
                                  css.cell = "padding-left: 3em; padding-right: 1em;")
                  )
            }
            if (kable){

                  ftbl$comp <- as.character(ftbl$comp)

                  ftbl$comp[!is.na(ftbl$comp)] <- paste("&nbsp; &nbsp; &nbsp; *", ftbl$comp[!is.na(ftbl$comp)])

                  ftbl$comp[is.na(ftbl$comp)] <- vnames

                  names(ftbl) <- head

                  if (title != "") footnote <- paste(title, "; ", footnote, sep="")

                  print(
                        kable(ftbl,
                              row.names = FALSE,
                              caption = footnote,
                              align = substr(alignr, 2, nchar(alignr)))
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

                  dfj <- df[complete.cases(df[,c(out, covs[j], randvars)]),]

                  for (r in 1:length(randvars)) dfj[,randvars[r]] <- factor(dfj[,randvars[r]])

                  if ("factor" %in% class(dfj[,covs[j]])) dfj[,covs[j]] <- droplevels(dfj[,covs[j]])

                  formj <- as.formula(paste(paste(out, "~", covs[j]),
                                           "+",
                                           random))
                  fitj <- glmer(formj
                               ,data = dfj
                               ,family = family
                               ,control = glmerControl(optimizer = optimizer)
                               )

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

            ftbl <- ftbl[,names(ftbl) != "op_value"]
            head <- c("Variable", estname, "95% CI", "p-value")
            alignr <- "llccr"

            if (!is.na(labels[1]) & sum(!is.na(labels)) == length(vnames_uni)){
                  vnames_uni <- labels
            }

            footnote <- paste("Family =", simcap(as.character(summary(fitj)$family)))
            if (!is.na(random)){
                  footnote <- paste("Mixed effecs regression, Family =", simcap(as.character(summary(fitj)$family)))
            }
            if (!footer) footnote <- ""

            vnames_uni <- paste(vnames_uni, nobs_uni)

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

                  ftbl$comp <- as.character(ftbl$comp)

                  ftbl$comp[!is.na(ftbl$comp)] <- paste("&nbsp; &nbsp; &nbsp; *", ftbl$comp[!is.na(ftbl$comp)])

                  ftbl$comp[is.na(ftbl$comp)] <- vnames_uni

                  names(ftbl) <- head

                  if (title != "") footnote <- paste(title, "; ", footnote, sep="")

                  print(
                        kable(ftbl,
                              row.names = FALSE,
                              caption = footnote,
                              align = substr(alignr, 2, nchar(alignr)))
                  )
            }

      }

      return(ftbl)

}
