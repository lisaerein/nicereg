‘nicereg’ R package examples
================
Lisa Rein
2019-07-12

### Load R packages

``` r
packs <- c("devtools",
           "knitr",
           "MASS",
           "geepack",
           "htmlTable")

lapply(packs, function(x) require(x, character.only = T))

install_github("lisaerein/nicereg")
library(nicereg)

help(nicereg)

options(knitr.kable.NA = '.')
```

### Load *survey* example dataset from the MASS package

``` r
head(survey <- survey)
```

    ##      Sex Wr.Hnd NW.Hnd W.Hnd    Fold Pulse    Clap Exer Smoke Height
    ## 1 Female   18.5   18.0 Right  R on L    92    Left Some Never 173.00
    ## 2   Male   19.5   20.5  Left  R on L   104    Left None Regul 177.80
    ## 3   Male   18.0   13.3 Right  L on R    87 Neither None Occas     NA
    ## 4   Male   18.8   18.9 Right  R on L    NA Neither None Never 160.00
    ## 5   Male   20.0   20.0 Right Neither    35   Right Some Never 165.00
    ## 6 Female   18.0   17.7 Right  L on R    64   Right Some Never 172.72
    ##        M.I    Age
    ## 1   Metric 18.250
    ## 2 Imperial 17.583
    ## 3     <NA> 16.917
    ## 4   Metric 20.333
    ## 5   Metric 23.667
    ## 6 Imperial 21.000

### Example: niceglm function for glm models

#### Run univariate linear regression and create a table

``` r
tab <- niceglm(df = survey,
                 out = "Height",
                 covs = c("Sex", "Age", "Clap"),
                 regtype = "uni",
                 title = "Univariate results",
                 estname = "Estimate",
                 family = "gaussian")
```

    ## Waiting for profiling to be done...
    ## Waiting for profiling to be done...
    ## Waiting for profiling to be done...

| Variable            | Estimate |      95% CI      |  p-value |
| :------------------ | :------: | :--------------: | -------: |
| Sex (N = 208)       |    .     |        .         |        . |
| \* Male vs. Female  |  13.14   | \[11.14, 15.14\] | \< 0.001 |
| Age (N = 209)       |  \-0.05  | \[-0.26, 0.15\]  |    0.592 |
| Clap (N = 208)      |    .     |        .         |        . |
| \* Neither vs. Left |  \-2.35  | \[-6.87, 2.17\]  |    0.310 |
| \* Right vs. Left   |  \-2.49  | \[-6.30, 1.32\]  |    0.202 |

Univariate results; Family = Gaussian (Identity)

#### Run multivariable linear regression and create a table

``` r
tab <- niceglm(df = survey,
                 out = "Height",
                 covs = c("Sex", "Age", "Clap"),
                 regtype = "multi",
                 title = "Multivariable results",
                 estname = "Estimate",
                 family = "gaussian")
```

    ## Waiting for profiling to be done...

| Variable            | Estimate |      95% CI      |  p-value |
| :------------------ | :------: | :--------------: | -------: |
| Sex                 |    .     |        .         |        . |
| \* Male vs. Female  |  13.33   | \[11.33, 15.34\] | \< 0.001 |
| Age                 |  \-0.04  | \[-0.19, 0.11\]  |    0.628 |
| Clap                |    .     |        .         |        . |
| \* Neither vs. Left |  \-2.13  | \[-5.50, 1.24\]  |    0.217 |
| \* Right vs. Left   |  \-3.30  | \[-6.14, -0.46\] |    0.024 |

Multivariable results; Family = Gaussian (Identity), N obs = 207

#### Create a results table from an existing glm model object

``` r
survey$lefthanded <- 0
survey$lefthanded[survey$W.Hnd == "Left"] <- 1

fit <- glm(lefthanded ~ Age + Sex + Height + Exer, data = survey, family = "binomial")

tab <- niceglm(fit,
               estname = "aOR",
               overallp = T)
```

    ## Waiting for profiling to be done...

| Variable           | aOR  |     95% CI      | Wald p-value | LR p-value |
| :----------------- | :--: | :-------------: | -----------: | ---------: |
| Age                | 0.81 | \[0.52, 1.00\]  |        0.218 |      0.069 |
| Sex                |  .   |        .        |            . |      0.868 |
| \* Male vs. Female | 0.88 | \[0.18, 4.07\]  |        0.868 |          . |
| Height             | 1.05 | \[0.97, 1.13\]  |        0.228 |      0.219 |
| Exer               |  .   |        .        |            . |      0.235 |
| \* None vs. Freq   | 4.31 | \[0.78, 20.88\] |        0.072 |          . |
| \* Some vs. Freq   | 1.49 | \[0.43, 5.14\]  |        0.517 |          . |

Family = Binomial (Logit), N obs = 208

### Example: nicegee function for geeglm models

``` r
survey$clusterid <- rep(1:5, length = nrow(survey))

tab <- nicegee(df = survey,
                 out = "Height",
                 covs = c("Sex", "Age", "Clap"),
                 regtype = "uni",
                 title = "Uni GEE results",
                 estname = "Estimate",
                 family = "gaussian",
                 id = "clusterid",
                 corstr = "exch")
```

| Variable                       | Estimate |      95% CI      |  p-value |
| :----------------------------- | :------: | :--------------: | -------: |
| Sex (N = 208, N clusters = 5)  |    .     |        .         |        . |
| \* Male vs. Female             |  13.14   | \[11.16, 15.12\] | \< 0.001 |
| Age (N = 209, N clusters = 5)  |  \-0.05  | \[-0.18, 0.07\]  |    0.400 |
| Clap (N = 208, N clusters = 5) |    .     |        .         |        . |
| \* Neither vs. Left            |  \-2.35  | \[-6.97, 2.28\]  |    0.320 |
| \* Right vs. Left              |  \-2.49  | \[-6.40, 1.42\]  |    0.212 |

Uni GEE results; Family = Gaussian, Corstr = Exchangeable

``` r
survey$clusterid <- rep(1:5, length = nrow(survey))

tab <- nicegee(df = survey,
                 out = "Height",
                 covs = c("Sex", "Age", "Clap"),
                 regtype = "multi",
                 title = "Multi GEE results",
                 estname = "Estimate",
                 family = "gaussian",
                 id = "clusterid",
                 corstr = "exch")
```

| Variable            | Estimate |      95% CI      |  p-value |
| :------------------ | :------: | :--------------: | -------: |
| Sex                 |    .     |        .         |        . |
| \* Male vs. Female  |  13.33   | \[11.34, 15.33\] | \< 0.001 |
| Age                 |  \-0.04  | \[-0.14, 0.07\]  |    0.480 |
| Clap                |    .     |        .         |        . |
| \* Neither vs. Left |  \-2.13  | \[-5.51, 1.26\]  |    0.218 |
| \* Right vs. Left   |  \-3.30  | \[-6.15, -0.44\] |    0.023 |

Multi GEE results; Family = Gaussian, N obs = 207, N clusters = 5,
Corstr = Exchangeable
