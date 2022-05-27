
<!-- README.md is generated from README.Rmd. Please edit that file -->

# srlTS

<!-- badges: start -->

[![R-CMD-check](https://github.com/petersonR/srlTS/workflows/R-CMD-check/badge.svg)](https://github.com/petersonR/srlTS/actions)
[![Codecov test
coverage](https://codecov.io/gh/petersonR/srlTS/branch/main/graph/badge.svg)](https://app.codecov.io/gh/petersonR/srlTS?branch=main)
<!-- badges: end -->

The goal of srlTS is to fit the sparsity-ranked lasso to time series
data.

## Installation

You can install the development version of srlTS like so:

``` r
# install.packages("remotes")
remotes::install_github("PetersonR/srlTS")
```

## Examples

This is a basic example.

``` r
library(srlTS)

y <- cumsum(rnorm(100))
fit <- srlTS(y, gamma = c(0, .5))

fit
#>  PF_gamma best_AICc best_BIC
#>       0.0  199.2616 209.5665
#>       0.5  197.1688 205.5474
#> 
#> Test-set prediction accuracy
#>         rmse       rsq       mae
#> AIC 1.170888 0.9464062 0.9735228
#> BIC 1.170888 0.9464062 0.9735228
```

For the Lake Huron data set

``` r
data("LakeHuron")

fit_LH <- srlTS(LakeHuron)

fit_LH
#>  PF_gamma best_AICc best_BIC
#>      0.00  147.2053 159.8118
#>      0.25  146.3811 156.7937
#>      0.50  146.0177 156.4739
#>      1.00  144.1002 154.4967
#>      2.00  143.0362 153.2544
#>      4.00  149.8227 156.1558
#>      8.00  149.8227 156.1558
#>     16.00  149.8227 156.1558
#> 
#> Test-set prediction accuracy
#>       rmse       rsq       mae
#> AIC 0.7751 0.6203825 0.5888855
#> BIC 0.7751 0.6203825 0.5888855
coef(fit_LH)
#>                 0.00069
#> (Intercept) 111.8740292
#> lag1          1.1003545
#> lag2         -0.4732437
#> lag3          0.1796316
#> lag4          0.0000000
#> lag5          0.0000000
#> lag6          0.0000000
#> lag7          0.0000000
#> lag8          0.0000000
#> lag9          0.0000000
```

If you have a univariate time series with suspected trend, such as the
EuStockMarkets data set,

``` r
data("EuStockMarkets")
X <- as.numeric(time(EuStockMarkets))
X_sp <- splines::bs(X-min(X), df = 9)

fit_stock <- srlTS(log(EuStockMarkets[,1]), n_lags_max = 400, X = X_sp, w_exo = "unpenalized")
tail(coef(fit_stock), 11)
#>           0.0000096
#> lag399  0.000000000
#> lag400  0.000000000
#> 1       0.251818563
#> 2      -0.064008340
#> 3      -0.022147416
#> 4      -0.015421857
#> 5      -0.014734876
#> 6       0.001020223
#> 7       0.011708216
#> 8       0.311147538
#> 9       0.000000000

# insert plot? 
```

If you have a univariate time series with suspected trend, such as the
airquality data set,

``` r
data("nottem")
fit_nt <- srlTS(nottem, n_lags_max = 24)
coef(fit_nt)
#>                  0.0393
#> (Intercept) 11.89830169
#> lag1         0.40437830
#> lag2         0.00000000
#> lag3        -0.06838188
#> lag4        -0.09660138
#> lag5        -0.01640032
#> lag6         0.00000000
#> lag7        -0.04079986
#> lag8         0.00000000
#> lag9         0.00000000
#> lag10        0.00000000
#> lag11        0.16707390
#> lag12        0.00000000
#> lag13        0.05894906
#> lag14        0.00000000
#> lag15        0.00000000
#> lag16        0.00000000
#> lag17        0.00000000
#> lag18        0.00000000
#> lag19        0.00000000
#> lag20        0.00000000
#> lag21        0.00000000
#> lag22        0.00000000
#> lag23        0.00000000
#> lag24        0.34816025

data("UKDriverDeaths")
fit_ukdd <- srlTS(UKDriverDeaths, n_lags_max = 24)
coef(fit_ukdd)
#>                  0.0282
#> (Intercept) 198.6573213
#> lag1          0.3805100
#> lag2          0.0000000
#> lag3          0.0000000
#> lag4          0.0000000
#> lag5          0.0000000
#> lag6          0.0000000
#> lag7          0.0000000
#> lag8          0.0000000
#> lag9          0.0000000
#> lag10         0.0000000
#> lag11         0.1645141
#> lag12         0.4682378
#> lag13         0.0000000
#> lag14        -0.1357345
#> lag15         0.0000000
#> lag16         0.0000000
#> lag17         0.0000000
#> lag18         0.0000000
#> lag19         0.0000000
#> lag20         0.0000000
#> lag21         0.0000000
#> lag22         0.0000000
#> lag23         0.0000000
#> lag24         0.0000000

# Show how to add holiday indicators?

data("sunspot.month")
fit_ssm <- srlTS(sunspot.month)
fit_ssm
#>  PF_gamma best_AICc best_BIC
#>      0.00  18373.20 18506.04
#>      0.25  18356.15 18467.11
#>      0.50  18348.28 18467.59
#>      1.00  18417.42 18502.81
#>      2.00  18569.61 18598.12
#>      4.00  18782.77 18799.88
#>      8.00  18782.77 18799.88
#>     16.00  18782.77 18799.88
#> 
#> Test-set prediction accuracy
#>         rmse       rsq      mae
#> AIC 15.94153 0.8920872 11.85384
#> BIC 16.04978 0.8906613 11.99382
summary(fit_ssm$relaxed_fit)
#> 
#> Call:
#> lm(formula = y ~ ., data = df)
#> 
#> Residuals:
#>     Min      1Q  Median      3Q     Max 
#> -53.463  -7.834  -1.291   6.984  88.595 
#> 
#> Coefficients:
#>              Estimate Std. Error t value Pr(>|t|)    
#> (Intercept)  3.029466   1.082016   2.800 0.005157 ** 
#> lag1         0.517812   0.021014  24.641  < 2e-16 ***
#> lag2         0.094300   0.023755   3.970 7.43e-05 ***
#> lag3         0.070096   0.024012   2.919 0.003545 ** 
#> lag4         0.085261   0.023876   3.571 0.000363 ***
#> lag5         0.042100   0.023980   1.756 0.079291 .  
#> lag6         0.070028   0.022170   3.159 0.001606 ** 
#> lag9         0.121889   0.019982   6.100 1.25e-09 ***
#> lag12        0.048008   0.019837   2.420 0.015595 *  
#> lag18       -0.046296   0.020300  -2.281 0.022669 *  
#> lag20       -0.024730   0.022534  -1.097 0.272563    
#> lag21       -0.024026   0.022477  -1.069 0.285225    
#> lag24       -0.024621   0.019845  -1.241 0.214864    
#> lag27       -0.021228   0.018908  -1.123 0.261680    
#> lag34        0.007448   0.016428   0.453 0.650318    
#> lag55       -0.005850   0.010639  -0.550 0.582474    
#> lag92        0.019619   0.016162   1.214 0.224909    
#> lag96        0.010579   0.017590   0.601 0.547619    
#> lag102       0.028248   0.017841   1.583 0.113500    
#> lag111       0.009525   0.014403   0.661 0.508480    
#> lag212      -0.030897   0.010286  -3.004 0.002695 ** 
#> lag275       0.014238   0.011115   1.281 0.200342    
#> lag292      -0.023290   0.010847  -2.147 0.031884 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#> 
#> Residual standard error: 14.83 on 2201 degrees of freedom
#> Multiple R-squared:  0.8864, Adjusted R-squared:  0.8853 
#> F-statistic: 780.9 on 22 and 2201 DF,  p-value: < 2.2e-16
```
