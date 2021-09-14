Project1 - Diabetes dataset analysis
================
true
11 September, 2021

-   [Dependencies](#dependencies)
-   [Data info](#data-info)
-   [Data preview](#data-preview)
-   [Data initial inspection](#data-initial-inspection)
    -   [Scatter plot](#scatter-plot)
    -   [Histogram](#histogram)
    -   [Q-Q plot](#q-q-plot)
    -   [Box plot](#box-plot)
    -   [Simple linear regression](#simple-linear-regression)
    -   [Correlation analysis](#correlation-analysis)

## Dependencies

``` r
library(dplyr)
library(ggplot2)
library(Hmisc)
```

## Data info

\[Data Sources\] (<https://www.kaggle.com/mathchi/diabetes-data-set>)

``` r
# import raw data
diabetes <- read.csv('diabetes.csv',header = T)
# replace 0 with NA
diabetes$Glucose[diabetes$Glucose == 0] <- NA
diabetes$BloodPressure[diabetes$BloodPressure == 0] <- NA
diabetes$SkinThickness[diabetes$SkinThickness == 0] <- NA
diabetes$Insulin[diabetes$Insulin == 0] <- NA
diabetes$BMI[diabetes$BMI == 0] <- NA
diabetes$Age[diabetes$Age == 0] <- NA
diabetes$Outcome[diabetes$Outcome == 0] <- 'healthy'
diabetes$Outcome[diabetes$Outcome == 1] <- 'diabetes'
# turn the outcome variable into factor
diabetes$Outcome <- as.factor(diabetes$Outcome)
# check the data structure
str(diabetes)
```

    ## 'data.frame':    768 obs. of  9 variables:
    ##  $ Pregnancies             : int  6 1 8 1 0 5 3 10 2 8 ...
    ##  $ Glucose                 : int  148 85 183 89 137 116 78 115 197 125 ...
    ##  $ BloodPressure           : int  72 66 64 66 40 74 50 NA 70 96 ...
    ##  $ SkinThickness           : int  35 29 NA 23 35 NA 32 NA 45 NA ...
    ##  $ Insulin                 : int  NA NA NA 94 168 NA 88 NA 543 NA ...
    ##  $ BMI                     : num  33.6 26.6 23.3 28.1 43.1 25.6 31 35.3 30.5 NA ...
    ##  $ DiabetesPedigreeFunction: num  0.627 0.351 0.672 0.167 2.288 ...
    ##  $ Age                     : int  50 31 32 21 33 30 26 29 53 54 ...
    ##  $ Outcome                 : Factor w/ 2 levels "diabetes","healthy": 1 2 1 2 1 2 1 2 1 1 ...

``` r
# Remove NA rows
diabetes <-diabetes %>% na.omit()
dim(diabetes)
```

    ## [1] 392   9

After remove the NA values, the samples number reduced from 768 to 392

## Data preview

``` r
knitr::kable(head(diabetes))
```

|     | Pregnancies | Glucose | BloodPressure | SkinThickness | Insulin |  BMI | DiabetesPedigreeFunction | Age | Outcome  |
|:----|------------:|--------:|--------------:|--------------:|--------:|-----:|-------------------------:|----:|:---------|
| 4   |           1 |      89 |            66 |            23 |      94 | 28.1 |                    0.167 |  21 | healthy  |
| 5   |           0 |     137 |            40 |            35 |     168 | 43.1 |                    2.288 |  33 | diabetes |
| 7   |           3 |      78 |            50 |            32 |      88 | 31.0 |                    0.248 |  26 | diabetes |
| 9   |           2 |     197 |            70 |            45 |     543 | 30.5 |                    0.158 |  53 | diabetes |
| 14  |           1 |     189 |            60 |            23 |     846 | 30.1 |                    0.398 |  59 | diabetes |
| 15  |           5 |     166 |            72 |            19 |     175 | 25.8 |                    0.587 |  51 | diabetes |

``` r
diabetes_lm <- diabetes[, -9]
```

## Data initial inspection

### Scatter plot

``` r
pairs(~Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age , col = diabetes$Outcome, data = diabetes)
```

![](Proj1-Diabetes_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Histogram

``` r
par(mfrow=c(2,4))
for (i in colnames(diabetes_lm)){
hist(diabetes_lm[[i]], main = i, xlab = NULL)
}
```

![](Proj1-Diabetes_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Q-Q plot

``` r
par(mfrow=c(2,4))
for (i in colnames(diabetes_lm)){
qqnorm(diabetes_lm[[i]], main = i);qqline(diabetes_lm[[i]], col = "red", lwd = 2)
}
```

![](Proj1-Diabetes_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Box plot

``` r
par(mfrow=c(2,4))
for (i in colnames(diabetes_lm)){
boxplot(diabetes_lm[[i]], main = i)
}
```

![](Proj1-Diabetes_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Simple linear regression

``` r
# function to make a pairwise linear regression between each variables
pairwise_lm <- function (dat) {
  n <- nrow(dat)
  p <- ncol(dat)
  LHS <- rep(colnames(dat), p)
  RHS <- rep(colnames(dat), each = p)
  ## function to fit and summarize a single model
  fitmodel <- function (LHS, RHS) {
    if (RHS == LHS) {
      z <- data.frame("LHS" = LHS, "RHS" = RHS,
                      "alpha" = 0,
                      "beta" = 1,
                      "beta.se" = 0,
                      "beta.tv" = Inf,
                      "beta.pv" = 0,
                      "sig" = 0,
                      "R2" = 1,
                      "F.fv" = Inf,
                      "F.pv" = 0,
                      stringsAsFactors = FALSE)
      } else {
      result <- summary(lm(reformulate(RHS, LHS), data = dat))
      z <- data.frame("LHS" = LHS, "RHS" = RHS,
                      "alpha" = result$coefficients[1, 1],
                      "beta" = result$coefficients[2, 1],
                      "beta.se" = result$coefficients[2, 2],
                      "beta.tv" = result$coefficients[2, 3],
                      "beta.pv" = result$coefficients[2, 4],
                      "sig" = result$sigma,
                      "R2" = result$r.squared,
                      "F.fv" = result$fstatistic[[1]],
                      "F.pv" = pf(result$fstatistic[[1]], 1, n - 2, lower.tail = FALSE),
                      stringsAsFactors = FALSE)
        }
      z
      }
  ## loop through all models
  do.call("rbind.data.frame", c(Map(fitmodel, LHS, RHS),
                                list(make.row.names = FALSE,
                                     stringsAsFactors = FALSE)))
  }
```

``` r
# perform pairwise linear regression
diabetes_lm_res <- pairwise_lm(diabetes_lm)
knitr::kable(diabetes_lm_res)
```

| LHS                      | RHS                      |        alpha |       beta |    beta.se |    beta.tv |   beta.pv |         sig |        R2 |        F.fv |      F.pv |
|:-------------------------|:-------------------------|-------------:|-----------:|-----------:|-----------:|----------:|------------:|----------:|------------:|----------:|
| Pregnancies              | Pregnancies              |    0.0000000 |  1.0000000 |  0.0000000 |        Inf | 0.0000000 |   0.0000000 | 1.0000000 |         Inf | 0.0000000 |
| Glucose                  | Pregnancies              |  116.3374082 |  1.9055147 |  0.4769429 |  3.9952676 | 0.0000773 |  30.2867384 | 0.0393193 |  15.9621634 | 0.0000773 |
| BloodPressure            | Pregnancies              |   67.9227826 |  0.8301926 |  0.1924986 |  4.3127205 | 0.0000205 |  12.2240082 | 0.0455203 |  18.5995581 | 0.0000205 |
| SkinThickness            | Pregnancies              |   28.1378311 |  0.3052320 |  0.1650985 |  1.8487868 | 0.0652453 |  10.4840562 | 0.0086880 |   3.4180125 | 0.0652453 |
| Insulin                  | Pregnancies              |  146.4076985 |  2.9228611 |  1.8680131 |  1.5646898 | 0.1184667 | 118.6222060 | 0.0062384 |   2.4482543 | 0.1184667 |
| BMI                      | Pregnancies              |   33.2693262 | -0.0554682 |  0.1107748 | -0.5007295 | 0.6168440 |   7.0344024 | 0.0006425 |   0.2507300 | 0.6168440 |
| DiabetesPedigreeFunction | Pregnancies              |    0.5203604 |  0.0008135 |  0.0054474 |  0.1493441 | 0.8813593 |   0.3459208 | 0.0000572 |   0.0223037 | 0.8813593 |
| Age                      | Pregnancies              |   23.7388501 |  2.1587100 |  0.1179907 | 18.2955953 | 0.0000000 |   7.4926225 | 0.4618677 | 334.7288082 | 0.0000000 |
| Pregnancies              | Glucose                  |    0.7706625 |  0.0206345 |  0.0051647 |  3.9952676 | 0.0000773 |   3.1516887 | 0.0393193 |  15.9621634 | 0.0000773 |
| Glucose                  | Glucose                  |    0.0000000 |  1.0000000 |  0.0000000 |        Inf | 0.0000000 |   0.0000000 | 1.0000000 |         Inf | 0.0000000 |
| BloodPressure            | Glucose                  |   60.2345792 |  0.0850436 |  0.0200465 |  4.2423144 | 0.0000277 |  12.2330280 | 0.0441112 |  17.9972318 | 0.0000277 |
| SkinThickness            | Glucose                  |   20.8356663 |  0.0677641 |  0.0169109 |  4.0071148 | 0.0000736 |  10.3196027 | 0.0395436 |  16.0569693 | 0.0000736 |
| Insulin                  | Glucose                  | -118.4125406 |  2.2382300 |  0.1586783 | 14.1054562 | 0.0000000 |  96.8306530 | 0.3378202 | 198.9638949 | 0.0000000 |
| BMI                      | Glucose                  |   27.2355105 |  0.0477113 |  0.0112752 |  4.2315256 | 0.0000290 |   6.8804862 | 0.0438969 |  17.9058088 | 0.0000290 |
| DiabetesPedigreeFunction | Glucose                  |    0.3306035 |  0.0015693 |  0.0005613 |  2.7959439 | 0.0054308 |   0.3425150 | 0.0196505 |   7.8173021 | 0.0054308 |
| Age                      | Glucose                  |   16.9357943 |  0.1135879 |  0.0157183 |  7.2264607 | 0.0000000 |   9.5918322 | 0.1180895 |  52.2217349 | 0.0000000 |
| Pregnancies              | BloodPressure            |   -0.5735145 |  0.0548310 |  0.0127138 |  4.3127205 | 0.0000205 |   3.1415006 | 0.0455203 |  18.5995581 | 0.0000205 |
| Glucose                  | BloodPressure            |   85.9752995 |  0.5186889 |  0.1222655 |  4.2423144 | 0.0000277 |  30.2111097 | 0.0441112 |  17.9972318 | 0.0000277 |
| BloodPressure            | BloodPressure            |    0.0000000 |  1.0000000 |  0.0000000 |        Inf | 0.0000000 |   0.0000000 | 1.0000000 |         Inf | 0.0000000 |
| SkinThickness            | BloodPressure            |   15.3147293 |  0.1957266 |  0.0414464 |  4.7224043 | 0.0000033 |  10.2411614 | 0.0540894 |  22.3011019 | 0.0000033 |
| Insulin                  | BloodPressure            |   89.8534887 |  0.9368748 |  0.4792308 |  1.9549554 | 0.0513023 | 118.4151563 | 0.0097045 |   3.8218505 | 0.0513023 |
| BMI                      | BloodPressure            |   20.9891696 |  0.1711930 |  0.0271262 |  6.3109838 | 0.0000000 |   6.7027259 | 0.0926614 |  39.8285167 | 0.0000000 |
| DiabetesPedigreeFunction | BloodPressure            |    0.5542483 | -0.0004416 |  0.0013998 | -0.3154443 | 0.7525931 |   0.3458866 | 0.0002551 |   0.0995051 | 0.7525931 |
| Age                      | BloodPressure            |   13.5574542 |  0.2449270 |  0.0394314 |  6.2114761 | 0.0000000 |   9.7432631 | 0.0900234 |  38.5824348 | 0.0000000 |
| Pregnancies              | SkinThickness            |    2.4714381 |  0.0284636 |  0.0153958 |  1.8487868 | 0.0652453 |   3.2015403 | 0.0086880 |   3.4180125 | 0.0652453 |
| Glucose                  | SkinThickness            |  105.6197850 |  0.5835487 |  0.1456282 |  4.0071148 | 0.0000736 |  30.2832026 | 0.0395436 |  16.0569693 | 0.0000736 |
| BloodPressure            | SkinThickness            |   62.6088843 |  0.2763516 |  0.0585193 |  4.7224043 | 0.0000033 |  12.1690123 | 0.0540894 |  22.3011019 | 0.0000033 |
| SkinThickness            | SkinThickness            |    0.0000000 |  1.0000000 |  0.0000000 |        Inf | 0.0000000 |   0.0000000 | 1.0000000 |         Inf | 0.0000000 |
| Insulin                  | SkinThickness            |   96.0470362 |  2.0589551 |  0.5626490 |  3.6593953 | 0.0002876 | 117.0021927 | 0.0331965 |  13.3911743 | 0.0002876 |
| BMI                      | SkinThickness            |   20.1468622 |  0.4439589 |  0.0252914 | 17.5537247 | 0.0000000 |   5.2593238 | 0.4413674 | 308.1332494 | 0.0000000 |
| DiabetesPedigreeFunction | SkinThickness            |    0.3693699 |  0.0052727 |  0.0016420 |  3.2112220 | 0.0014313 |   0.3414461 | 0.0257598 |  10.3119465 | 0.0014313 |
| Age                      | SkinThickness            |   26.1220849 |  0.1627258 |  0.0484210 |  3.3606453 | 0.0008544 |  10.0690921 | 0.0281438 |  11.2939365 | 0.0008544 |
| Pregnancies              | Insulin                  |    2.9679418 |  0.0021344 |  0.0013641 |  1.5646898 | 0.1184667 |   3.2054934 | 0.0062384 |   2.4482543 | 0.1184667 |
| Glucose                  | Insulin                  |   99.0737120 |  0.1509318 |  0.0107002 | 14.1054562 | 0.0000000 |  25.1449601 | 0.3378202 | 198.9638949 | 0.0000000 |
| BloodPressure            | Insulin                  |   69.0467749 |  0.0103584 |  0.0052985 |  1.9549554 | 0.0513023 |  12.4512420 | 0.0097045 |   3.8218505 | 0.0513023 |
| SkinThickness            | Insulin                  |   26.6293179 |  0.0161230 |  0.0044059 |  3.6593953 | 0.0002876 |  10.3536449 | 0.0331965 |  13.3911743 | 0.0002876 |
| Insulin                  | Insulin                  |    0.0000000 |  1.0000000 |  0.0000000 |        Inf | 0.0000000 |   0.0000000 | 1.0000000 |         Inf | 0.0000000 |
| BMI                      | Insulin                  |   30.9969648 |  0.0133879 |  0.0029166 |  4.5901553 | 0.0000060 |   6.8539579 | 0.0512554 |  21.0695261 | 0.0000060 |
| DiabetesPedigreeFunction | Insulin                  |    0.4613888 |  0.0003951 |  0.0001458 |  2.7090594 | 0.0070448 |   0.3427211 | 0.0184704 |   7.3390029 | 0.0070448 |
| Age                      | Insulin                  |   27.9569658 |  0.0186332 |  0.0042428 |  4.3917543 | 0.0000145 |   9.9702803 | 0.0471246 |  19.2875062 | 0.0000145 |
| Pregnancies              | BMI                      |    3.6842557 | -0.0115829 |  0.0231321 | -0.5007295 | 0.6168440 |   3.2145059 | 0.0006425 |   0.2507300 | 0.6168440 |
| Glucose                  | BMI                      |   92.1864433 |  0.9200538 |  0.2174284 |  4.2315256 | 0.0000290 |  30.2144951 | 0.0438969 |  17.9058088 | 0.0000290 |
| BloodPressure            | BMI                      |   52.7547257 |  0.5412688 |  0.0857661 |  6.3109838 | 0.0000000 |  11.9183179 | 0.0926614 |  39.8285167 | 0.0000000 |
| SkinThickness            | BMI                      |   -3.7476855 |  0.9941628 |  0.0566354 | 17.5537247 | 0.0000000 |   7.8702277 | 0.4413674 | 308.1332494 | 0.0000000 |
| Insulin                  | BMI                      |   29.3857405 |  3.8284931 |  0.8340661 |  4.5901553 | 0.0000060 | 115.9043020 | 0.0512554 |  21.0695261 | 0.0000060 |
| BMI                      | BMI                      |    0.0000000 |  1.0000000 |  0.0000000 |        Inf | 0.0000000 |   0.0000000 | 1.0000000 |         Inf | 0.0000000 |
| DiabetesPedigreeFunction | BMI                      |    0.2647956 |  0.0078054 |  0.0024578 |  3.1757600 | 0.0016133 |   0.3415427 | 0.0252082 |  10.0854518 | 0.0016133 |
| Age                      | BMI                      |   27.5119699 |  0.1013360 |  0.0733212 |  1.3820843 | 0.1677369 |  10.1889247 | 0.0048740 |   1.9101570 | 0.1677369 |
| Pregnancies              | DiabetesPedigreeFunction |    3.2642543 |  0.0702923 |  0.4706737 |  0.1493441 | 0.8813593 |   3.2154471 | 0.0000572 |   0.0223037 | 0.8813593 |
| Glucose                  | DiabetesPedigreeFunction |  116.0781691 | 12.5216194 |  4.4784946 |  2.7959439 | 0.0054308 |  30.5952110 | 0.0196505 |   7.8173021 | 0.0054308 |
| BloodPressure            | DiabetesPedigreeFunction |   70.9654107 | -0.5776651 |  1.8312746 | -0.3154443 | 0.7525931 |  12.5105061 | 0.0002551 |   0.0995051 | 0.7525931 |
| SkinThickness            | DiabetesPedigreeFunction |   26.5900844 |  4.8854673 |  1.5213733 |  3.2112220 | 0.0014313 |  10.3933891 | 0.0257598 |  10.3119465 | 0.0014313 |
| Insulin                  | DiabetesPedigreeFunction |  131.6041698 | 46.7491510 | 17.2565986 |  2.7090594 | 0.0070448 | 117.8898997 | 0.0184704 |   7.3390029 | 0.0070448 |
| BMI                      | DiabetesPedigreeFunction |   31.3969945 |  3.2296017 |  1.0169540 |  3.1757600 | 0.0016133 |   6.9474062 | 0.0252082 |  10.0854518 | 0.0016133 |
| DiabetesPedigreeFunction | DiabetesPedigreeFunction |    0.0000000 |  1.0000000 |  0.0000000 |        Inf | 0.0000000 |   0.0000000 | 1.0000000 |         Inf | 0.0000000 |
| Age                      | DiabetesPedigreeFunction |   29.5516662 |  2.5105439 |  1.4896774 |  1.6852936 | 0.0927318 |  10.1768563 | 0.0072299 |   2.8402147 | 0.0927318 |
| Pregnancies              | Age                      |   -3.3026695 |  0.2139554 |  0.0116944 | 18.2955953 | 0.0000000 |   2.3588391 | 0.4618677 | 334.7288082 | 0.0000000 |
| Glucose                  | Age                      |   90.5395480 |  1.0396311 |  0.1438645 |  7.2264607 | 0.0000000 |  29.0185190 | 0.1180895 |  52.2217349 | 0.0000000 |
| BloodPressure            | Age                      |   59.3188529 |  0.3675518 |  0.0591730 |  6.2114761 | 0.0000000 |  11.9356313 | 0.0900234 |  38.5824348 | 0.0000000 |
| SkinThickness            | Age                      |   23.8072722 |  0.1729523 |  0.0514640 |  3.3606453 | 0.0008544 |  10.3806647 | 0.0281438 |  11.2939365 | 0.0008544 |
| Insulin                  | Age                      |   77.9971572 |  2.5290614 |  0.5758659 |  4.3917543 | 0.0000145 | 116.1563490 | 0.0471246 |  19.2875062 | 0.0000145 |
| BMI                      | Age                      |   31.6017179 |  0.0480971 |  0.0348004 |  1.3820843 | 0.1677369 |   7.0194941 | 0.0048740 |   1.9101570 | 0.1677369 |
| DiabetesPedigreeFunction | Age                      |    0.4341604 |  0.0028798 |  0.0017088 |  1.6852936 | 0.0927318 |   0.3446779 | 0.0072299 |   2.8402147 | 0.0927318 |
| Age                      | Age                      |    0.0000000 |  1.0000000 |  0.0000000 |        Inf | 0.0000000 |   0.0000000 | 1.0000000 |         Inf | 0.0000000 |

### Correlation analysis

``` r
rcorr(as.matrix(diabetes_lm), type = "spearman")
```

    ##                          Pregnancies Glucose BloodPressure SkinThickness
    ## Pregnancies                     1.00    0.19          0.15          0.05
    ## Glucose                         0.19    1.00          0.24          0.22
    ## BloodPressure                   0.15    0.24          1.00          0.25
    ## SkinThickness                   0.05    0.22          0.25          1.00
    ## Insulin                         0.12    0.66          0.13          0.24
    ## BMI                            -0.07    0.20          0.32          0.67
    ## DiabetesPedigreeFunction        0.01    0.09         -0.02          0.09
    ## Age                             0.63    0.35          0.33          0.24
    ##                          Insulin   BMI DiabetesPedigreeFunction  Age
    ## Pregnancies                 0.12 -0.07                     0.01 0.63
    ## Glucose                     0.66  0.20                     0.09 0.35
    ## BloodPressure               0.13  0.32                    -0.02 0.33
    ## SkinThickness               0.24  0.67                     0.09 0.24
    ## Insulin                     1.00  0.30                     0.13 0.26
    ## BMI                         0.30  1.00                     0.10 0.17
    ## DiabetesPedigreeFunction    0.13  0.10                     1.00 0.10
    ## Age                         0.26  0.17                     0.10 1.00
    ## 
    ## n= 392 
    ## 
    ## 
    ## P
    ##                          Pregnancies Glucose BloodPressure SkinThickness
    ## Pregnancies                          0.0001  0.0025        0.2795       
    ## Glucose                  0.0001              0.0000        0.0000       
    ## BloodPressure            0.0025      0.0000                0.0000       
    ## SkinThickness            0.2795      0.0000  0.0000                     
    ## Insulin                  0.0147      0.0000  0.0091        0.0000       
    ## BMI                      0.1953      0.0000  0.0000        0.0000       
    ## DiabetesPedigreeFunction 0.8172      0.0773  0.6801        0.0657       
    ## Age                      0.0000      0.0000  0.0000        0.0000       
    ##                          Insulin BMI    DiabetesPedigreeFunction Age   
    ## Pregnancies              0.0147  0.1953 0.8172                   0.0000
    ## Glucose                  0.0000  0.0000 0.0773                   0.0000
    ## BloodPressure            0.0091  0.0000 0.6801                   0.0000
    ## SkinThickness            0.0000  0.0000 0.0657                   0.0000
    ## Insulin                          0.0000 0.0091                   0.0000
    ## BMI                      0.0000         0.0569                   0.0009
    ## DiabetesPedigreeFunction 0.0091  0.0569                          0.0416
    ## Age                      0.0000  0.0009 0.0416
