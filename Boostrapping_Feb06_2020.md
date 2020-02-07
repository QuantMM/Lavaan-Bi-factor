Lavaan on Bootstrapping Methods
================
Sunmee Kim
Feb 06, 2020

------------------------------------------------------------------------

Bootstrapping Applications in SEM
---------------------------------

The term 'bootstrapping' is a general statistical term to indicate any tests or measures based on random sampling (w/ replacement). Bootstrapping applications in SEM literature can have two different purposes.

#### (1) To approximate fit indices and their cut-points

-   Main reference: [Kim and Millsap (2014)](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4280787/)
-   Cut-point selection? Cut-points for different fit measures (e.g., RMSEA .05 for a close fit) that makes binary decisions on "so, the model fits well or not?"
-   The choice of cut-points depends on model complexity, number of measured variables, the specified model, distributional conditions, and sample size
-   pp.4-5: "In SEM, we formulate a covariance structure model. The Bollen-Stine (B-S) method (Bollen & Stine, 1993) provides a way of imposing the model on the sample data so that bootstrapping is done under that model."
-   In Lavaan, this task can be done by choosing different arguments of "test". See p.52 of [Package âlavaanâ](https://cran.r-project.org/web/packages/lavaan/lavaan.pdf)
-   Options = standard, Satorra.Bentler, Yuan.Bentler, mean.var.adjusted, Bollen.Stine

#### (2) To find bootstrap standard errors

-   Use bootstrapping as a very general method for estimating SEs and CIs for parameter estimates
-   In Lavaan, this is done with the "se" argument
-   Options = robust.sem, robust.huber.white, bootstrap
-   In the "bootstrap" argument, assign the number of bootstrap draws (default = 1000)

------------------------------------------------------------------------

#### Example

-   Model used for illustration

``` r
fit25 <- "
    L1 =~ Pren_CESD1 + ... + Pren_pregn_anx_q4
    L2 =~ ... 
    L3 =~ ...
    L4 =~ ...
    L5 =~ ...

    L10 =~ CBCL_emo_48m + ...
    L11 =~ ...
    L12 =~ ...
    L13 =~ ...
    
    L10 ~ L1 + L5
"
```

-   Try different argument options to see the output differences

``` r
Model1 <- lavaan::sem(fit25, data = m.data,
                    estimator = "ML", missing = "fiml", std.ov = TRUE, std.lv = TRUE, orthogonal = T
                    )

-------
  
Warning message:
In lav_model_vcov(lavmodel = lavmodel2, lavsamplestats = lavsamplestats,  :
  lavaan WARNING:
    The variance-covariance matrix of the estimated parameters (vcov)
    does not appear to be positive definite! The smallest eigenvalue
    (= -1.913616e+03) is smaller than zero. This may be a symptom that
    the model is not identified.

-------
  L13 =~                                                                
    SDQ_emo_dd_60m    0.613       NA                      0.613    0.600
    SDQ_per_dd_60m    0.262       NA                      0.262    0.260

Regressions:
                   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
  L10 ~                                                                 
    L1                0.333    0.070    4.772    0.000    0.312    0.312
    L5                0.172    0.078    2.192    0.028    0.161    0.161
-------
```

``` r
Model2 <- lavaan::sem(fit25, data = m.data,
                    estimator = "ML", missing = "fiml", std.ov = TRUE, std.lv = TRUE, orthogonal = T,
                    se = "bootstrap",
                    test = "standard"
                    )

# NO model fit measures; tried fitMeasures() but the rest of 
# the fit measures were the same as the results from Model1
-------
lavaan 0.6-5.1458 ended normally after 54 iterations

  Estimator                                         ML               
  Optimization method                           NLMINB               
  Number of free parameters                        174               
                                                                     
  Number of observations                           639
  Number of missing patterns                        98

  Model Fit Test Statistic                    1464.706
  Degrees of freedom                               815
  P-value (Chi-square)                           0.000

Parameter Estimates:

  Standard Errors                            Bootstrap
  Number of requested bootstrap draws             1000
  Number of successful bootstrap draws             989
-------
  L13 =~                                              
    SDQ_emo_dd_60m    0.613    0.095    6.467    0.000
    SDQ_per_dd_60m    0.262    0.231    1.135    0.256

Regressions:
                   Estimate  Std.Err  z-value  P(>|z|)
  L10 ~                                               
    L1                0.333    0.098    3.409    0.001
    L5                0.172    0.080    2.160    0.031
-------
```

``` r
Model3 <- lavaan::sem(fit25, data = m.data,
                    estimator = "ML", missing = "fiml", std.ov = TRUE, std.lv = TRUE, orthogonal = T,
                    se = "bootstrap",
                    test = "Bollen.Stine")

Error in bootstrap.internal(object = NULL, lavmodel. = lavmodel, lavsamplestats. = lavsamplestats,  : 
  lavaan ERROR: bollen.stine/yuan bootstrap not available for missing data

# Error occurred: missing value imputation 
# It seems the option se = "bootstrap" is only for the second purpose (described above)
```

``` r
Model4 <- lavaan::sem(fit25, data = m.data,
                    estimator = "ML", missing = "fiml", std.ov = TRUE, std.lv = TRUE, orthogonal = T,
                    se = "bootstrap",
                    test = "Yuan.Bentler",
                    bootstrap = 100 # default = 1000
)

# NO model fit measures; tried fitMeasures() but same as the results from Model1

--------
  lavaan 0.6-5.1458 ended normally after 54 iterations

  Estimator                                         ML               
  Optimization method                           NLMINB               
  Number of free parameters                        174               
                                                                     
  Number of observations                           639
  Number of missing patterns                        98

  Model Fit Test Statistic                    1464.706    1189.140
  Degrees of freedom                               815         815
  P-value (Chi-square)                           0.000       0.000
  Scaling correction factor                                  1.232
    for the Yuan-Bentler correction

Parameter Estimates:

  Standard Errors                            Bootstrap
  Number of requested bootstrap draws              100
  Number of successful bootstrap draws              99

---------

  L13 =~                                              
    SDQ_emo_dd_60m    0.613    0.068    9.065    0.000
    SDQ_per_dd_60m    0.262    0.121    2.170    0.030

Regressions:
                   Estimate  Std.Err  z-value  P(>|z|)
  L10 ~                                               
    L1                0.333    0.080    4.188    0.000
    L5                0.172    0.084    2.047    0.041

---------
```

``` r
Model5 <- lavaan::sem(fit25, data = m.data,
                    estimator = "ML", missing = "fiml", std.ov = TRUE, std.lv = TRUE, orthogonal = T,
                    test = "Yuan.Bentler"
)

Warning message:
In lav_model_vcov(lavmodel = lavmodel2, lavsamplestats = lavsamplestats,  :
  lavaan WARNING:
    The variance-covariance matrix of the estimated parameters (vcov)
    does not appear to be positive definite! The smallest eigenvalue
    (= -1.913616e+03) is smaller than zero. This may be a symptom that
    the model is not identified.

----------
  Estimator                                         ML               
  Optimization method                           NLMINB               
  Number of free parameters                        174               
                                                                     
  Number of observations                           639
  Number of missing patterns                        98

  Model Fit Test Statistic                    1464.706    1189.140
  Degrees of freedom                               815         815
  P-value (Chi-square)                           0.000       0.000
  Scaling correction factor                                  1.232
    for the Yuan-Bentler correction

Parameter Estimates:

  Information                                 Observed
  Observed information based on                Hessian
  Standard Errors                             Standard
----------
  L13 =~                                              
    SDQ_emo_dd_60m    0.613       NA                  
    SDQ_per_dd_60m    0.262       NA                  

Regressions:
                   Estimate  Std.Err  z-value  P(>|z|)
  L10 ~                                               
    L1                0.333    0.070    4.772    0.000
    L5                0.172    0.078    2.192    0.028
----------
      
# Basically same as Model1, but corrected Chi-square
```

``` r
Model6 <- lavaan::sem(fit25, data = m.data,
                    estimator = "ML", missing = "fiml", std.ov = TRUE, std.lv = TRUE, orthogonal = T,
                    se = "bootstrap",
                    test = "Bollen.Stine",
                    bootstrap = 100 # default = 1000
)

Error in bootstrap.internal(object = NULL, lavmodel. = lavmodel, lavsamplestats. = lavsamplestats,  : 
  lavaan ERROR: bollen.stine/yuan bootstrap not available for missing data

# Error occurred: missing value imputation 
```

``` r
# Notice: missing = "two.stage"

Model7 <- lavaan::sem(fit25, data = m.data,
                    estimator = "ML", missing = "two.stage", std.ov = TRUE, std.lv = TRUE, orthogonal = T,
                    se = "bootstrap",
                    test = "Bollen.Stine",
                    bootstrap = 100 # default = 1000
)

------------
lavaan 0.6-5.1458 ended normally after 38 iterations

  Estimator                                         ML               
  Optimization method                           NLMINB               
  Number of free parameters                        174               
                                                                     
  Number of observations                           639
  Number of missing patterns                        98

  Model Fit Test Statistic                    3942.259
  Degrees of freedom                               815
  P-value (Chi-square)                           0.000

Parameter Estimates:

  Information                                 Observed
  Information saturated (h1) model          Structured
  Observed information based on                     H1
  Standard Errors                            Two.stage

Latent Variables:
                   Estimate  Std.Err  z-value  P(>|z|)
  L1 =~                                               
    Pren_CESD1        0.505       NA                  
    Pren_CESD2        0.414       NA                  
------------

# Run, but results were all NAs
```

------------------------------------------------------------------------

-   If the purpose of applying bootstrapping in the present analysis is to obtain bootstrap SE for different parameter estimates, use the "se" argument (this can solve the NA issues in Model1).

-   BUT, based on the description on "missing" (p.49 of [Package âlavaanâ](https://cran.r-project.org/web/packages/lavaan/lavaan.pdf)), not sure how Lavaan incorporates bootstrapping and missing value imputation. It seems that the missing value procedure comes first (i.e., random sampling based on the imputed/complete data)

-   If we focus on the first purpose, then we should find a way to implement both missing value imputation and test = "Bollen.Stine"

-   Maybe, need to take a look [here](https://rdrr.io/cran/lavaan/man/bootstrap.html)!

``` r
# fit the Holzinger and Swineford (1939) example
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data=HolzingerSwineford1939, se="none")

# get the test statistic for the original sample
T.orig <- fitMeasures(fit, "chisq")

# bootstrap to get bootstrap test statistics
# we only generate 10 bootstrap sample in this example; in practice
# you may wish to use a much higher number
T.boot <- bootstrapLavaan(fit, R=10, type="bollen.stine",
                          FUN=fitMeasures, fit.measures="chisq")

# compute a bootstrap based p-value
pvalue.boot <- length(which(T.boot > T.orig))/length(T.boot)
```

------------------------------------------------------------------------
