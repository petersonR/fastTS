# Simple Case Studies

``` r
library(fastTS)
library(magrittr) # for pipe
```

## Lake Huron data set

``` r
data("LakeHuron")

fit_LH <- fastTS(LakeHuron)

fit_LH
#> An endogenous PACF-based fastTS model.
#> 
#>  PF_gamma AICc_d BIC_d
#>      0.00   4.17  6.56
#>      0.25   3.34  3.54
#>      0.50   2.98  3.22
#>      1.00   1.06  1.24
#>      2.00    *0*   *0*
#>      4.00   6.79   2.9
#>      8.00   6.79   2.9
#>     16.00   6.79   2.9
#> 
#> AICc_d and BIC_d are the difference from the minimum; *0* is best.
#> 
#> - Best AICc model: 4 active terms
#> - Best BIC  model: 4 active terms
#> 
#> Test-set prediction accuracy (20% held-out test set)
#>        rmse       rsq       mae
#> AICc 0.7751 0.6043019 0.5888855
#> BIC  0.7751 0.6043019 0.5888855
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

## EuStockMarkets

If you have a univariate time series with suspected trend, such as the
EuStockMarkets data set,

``` r
data("EuStockMarkets")
X <- as.numeric(time(EuStockMarkets))
X_sp <- splines::bs(X-min(X), df = 9)

fit_stock <- fastTS(log(EuStockMarkets[,1]), n_lags_max = 400, X = X_sp, w_exo = "unpenalized")
fit_stock
#> A PACF-based fastTS model with 9 exogenous features.
#> 
#>  PF_gamma AICc_d BIC_d
#>      0.00  11.36 28.22
#>      0.25   2.18  4.41
#>      0.50    *0*  0.74
#>      1.00   6.46 <0.01
#>      2.00   6.46   *0*
#>      4.00   6.46 <0.01
#>      8.00   6.46 <0.01
#>     16.00   6.46 <0.01
#> 
#> AICc_d and BIC_d are the difference from the minimum; *0* is best.
#> 
#> - Best AICc model: 18 active terms
#> - Best BIC  model: 10 active terms
#> 
#> Test-set prediction accuracy (20% held-out test set)
#>            rmse       rsq       mae
#> AICc 0.09071464 0.7328887 0.0758487
#> BIC  0.09292579 0.7197084 0.0780460
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

## Seasonal examples

### Nottem

``` r
data("nottem")
fit_nt <- fastTS(nottem, n_lags_max = 24)
fit_nt
#> An endogenous PACF-based fastTS model.
#> 
#>  PF_gamma AICc_d  BIC_d
#>      0.00   3.86   13.5
#>      0.25   0.43    *0*
#>      0.50    *0*   3.74
#>      1.00   2.49   3.91
#>      2.00  29.29  33.52
#>      4.00  83.31  75.53
#>      8.00 106.95  99.16
#>     16.00 212.75 201.97
#> 
#> AICc_d and BIC_d are the difference from the minimum; *0* is best.
#> 
#> - Best AICc model: 9 active terms
#> - Best BIC  model: 6 active terms
#> 
#> Test-set prediction accuracy (20% held-out test set)
#>          rmse       rsq      mae
#> AICc 2.324769 0.9223185 1.747609
#> BIC  2.376213 0.9188426 1.815714
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
```

### UKDriverDeaths

``` r
data("UKDriverDeaths")
fit_ukdd <- fastTS(UKDriverDeaths, n_lags_max = 24)
fit_ukdd
#> An endogenous PACF-based fastTS model.
#> 
#>  PF_gamma AICc_d BIC_d
#>      0.00   5.97   7.9
#>      0.25   3.91  3.91
#>      0.50   2.25  2.25
#>      1.00   0.77  0.77
#>      2.00   0.02  0.02
#>      4.00    *0*   *0*
#>      8.00  10.21  7.55
#>     16.00  39.18 33.83
#> 
#> AICc_d and BIC_d are the difference from the minimum; *0* is best.
#> 
#> - Best AICc model: 5 active terms
#> - Best BIC  model: 5 active terms
#> 
#> Test-set prediction accuracy (20% held-out test set)
#>          rmse       rsq      mae
#> AICc 170.9203 0.5776374 131.9969
#> BIC  170.9203 0.5776374 131.9969
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
```

### sunspot

``` r

data("sunspot.month")
fit_ssm <- fastTS(sunspot.month)
fit_ssm
#> An endogenous PACF-based fastTS model.
#> 
#>  PF_gamma AICc_d  BIC_d
#>      0.00  18.62  26.13
#>      0.25   5.77    3.5
#>      0.50    *0*    *0*
#>      1.00  55.68  30.84
#>      2.00 234.71 146.86
#>      4.00 460.98 361.64
#>      8.00 460.98 361.64
#>     16.00 460.98 361.64
#> 
#> AICc_d and BIC_d are the difference from the minimum; *0* is best.
#> 
#> - Best AICc model: 24 active terms
#> - Best BIC  model: 18 active terms
#> 
#> Test-set prediction accuracy (20% held-out test set)
#>          rmse       rsq      mae
#> AICc 22.04138 0.8969860 16.11950
#> BIC  22.05650 0.8968446 16.17492
```

Model summaries

``` r
summary(fit_ssm)
#> Model summary (ncvreg) at optimal AICc (lambda=0.0924; gamma=0.5)
#> lasso-penalized linear regression with n=2317, p=331
#> At lambda=0.0924:
#> -------------------------------------------------
#>   Nonzero coefficients         :  23
#>   Expected nonzero coefficients:  11.40
#>   Average mfdr (23 features)   :   0.496
#> 
#>          Estimate       z       mfdr Selected
#> lag1    0.5381873 73.8992    < 1e-04        *
#> lag9    0.1022791 14.7795    < 1e-04        *
#> lag2    0.0983075 13.8168    < 1e-04        *
#> lag4    0.0889776 12.6824    < 1e-04        *
#> lag3    0.0748760 10.6776    < 1e-04        *
#> lag6    0.0574307  8.6278    < 1e-04        *
#> lag5    0.0356911  5.5714    < 1e-04        *
#> lag18  -0.0336397 -5.1736 0.00011438        *
#> lag102  0.0242850  4.3714 0.00527675        *
#> lag27  -0.0203566 -3.5944 0.05843986        *
#> lag21  -0.0198168 -3.4229 0.09031381        *
#> lag212 -0.0170930 -3.1816 0.27456916        *
#> lag20  -0.0009492 -0.8997 0.96979786        *
#> lag12   0.0141200  2.7259 1.00000000        *
#> lag24  -0.0128349 -2.4821 1.00000000        *
#> lag34  -0.0086503 -2.0163 1.00000000        *
#> lag55  -0.0011088 -1.0797 1.00000000        *
#> lag92   0.0077636  1.8327 1.00000000        *
#> lag96   0.0012431  1.0838 1.00000000        *
#> lag111  0.0104439  2.3428 1.00000000        *
#> lag112  0.0046870  1.8807 1.00000000        *
#> lag116  0.0016910  1.4641 1.00000000        *
#> lag292 -0.0100337 -2.0345 1.00000000        *
```
