Lavaan Analysis Summary: Aug-Dec, 2019
================
Sunmee Kim
Jan 24, 2020

<style type="text/css">

h1.title {
  font-size: 30px;
  /* color: DarkRed; */
  text-align: center;
}
h4.author {
  font-size: 24px;
  /* font-family: "Times New Roman", Times, serif; */
  /* color: DarkRed; */
  text-align: center;
}
h4.date {
  font-size: 24px;
  text-align: center;
}
</style>

------------------------------------------------------------------------

1. Data Analysis Outline
------------------------

### - Goal of Study

-   (by Katie)
-   To date, results from one study indicate a significant contribution of maternal prenatal depression and child genetic susceptibility on infant negative emotionality
-   In a second study, results show that negative emotionality mediates the relationship between high pregnancy anxiety and childhood internalizing problems.
-   The goal of the final study will be to understand the combined contribution of these factors in a bigger model to determine if the mediation model identified in study two is moderated by child genetic susceptibly.

### - Final Model Specification (Dec, 2019)

##### (1) General Affective Psychopathology (GAP): Model25

-   Bifactor model
    -   General factor: General affective psychopathology (**<span style="color:blue">L1</span>**), a total of 18 variables load on **<span style="color:blue">L1</span>**
    -   Four group factors: Somatic symptoms (**<span style="color:blue">L2</span>**), Negative affect (**<span style="color:blue">L3</span>**), Anhedonia (**<span style="color:blue">L4</span>**), and Pregnancy anxiety (**<span style="color:blue">L5</span>**)

##### (2) General Internalizing Psychopathology: Models25

-   Bifactor model
    -   General factor: A total of 21 variables load on **<span style="color:blue">L10</span>**
    -   Group factors: Mother-rated (**<span style="color:blue">L11</span>**), Child-rated (**<span style="color:blue">L12</span>**), and Father-rated (**<span style="color:blue">L13</span>**)

##### (3) Moderation

-   Variables used: LITTLE ZIPPER proteins (ZPRS)
    -   PRS SDQ: ZqPRS\_0.0001 (SDQ: Strengths and Difficulties Questionnaire-assess child and adolescent mental health problems)
    -   PRS MDD: ZPRS\_0.4 (MDD: Major depressive disorder)
-   Tested for both the SDQ and MDD - in separate models
-   To use {Lavaan} in R, created the product terms, e.g., in the case of SDQ:

``` r
m.data$prod1    =   m.data$Pren_CESD1 * m.data$ZqPRS_0.0001
m.data$prod2    =   m.data$Pren_CESD2 * m.data$ZqPRS_0.0001
...
m.data$prod18   =   m.data$Pren_pregn_anx_q4 * m.data$ZqPRS_0.0001
# Created a total of 18 product terms: using 18 indicators (in GAP) && SDQ
```

-   and then, used those product terms as indicators of a latent variable (that represents the interaction term), e.g.,:

``` r
INT_G_SDQ =~ prod1+prod2+prod3+...+prod18
INT_AX_SDQ =~ prod15+prod16+prod17+prod18
```

-   -   <span style="color:blue">INT\_G\_SDQ</span>: interaction between the GAP general factor (<span style="color:blue">L1</span>) & SDQ
    -   <span style="color:blue">INT\_AX\_SDQ</span>: interaction between Pregnancy anxiety (<span style="color:blue">L5</span>) & SDQ

##### (4) Mediation

-   Variables used: NE\_18m, NE\_36m
-   In the final model, created a latent (**<span style="color:blue">NE</span>**) using both NE\_18 and NE\_36

------------------------------------------------------------------------

2. Model Specification in {Lavaan}
----------------------------------

### (1) SDQ model

``` r
fit25_SDQ <- "
    L1 =~ Pren_CESD1 + ... + Pren_pregn_anx_q4
    L2 =~ Pren_CESD1 + Pren_CESD2 + Pren_CESD5 + Pren_CESD7 + Pren_CESD11 + Pren_CESD20 
    L3 =~ Pren_CESD3 + Pren_CESD6 + Pren_CESD14 + Pren_CESD18
    L4 =~ Pren_CESD4_rec + Pren_CESD8_rec + Pren_CESD12_rec + Pren_CESD16_rec
    L5 =~ Pren_pregn_anx_q1 + Pren_pregn_anx_q2 + Pren_pregn_anx_q3 + Pren_pregn_anx_q4

    L10 =~ CBCL_emo_48m + ... + SDQ_peer_dad_60m
    L11 =~ CBCL_emo_48m + ... + SDQ_peer_mom_72m
    L12 =~ Dominic_sepanx + Dominic_overanx + Dominic_phobia + Dominic_mdd
    L13 =~ SDQ_emo_dad_60m + SDQ_peer_dad_60m
    
    NE =~ NE_18m + NE_36m
    
    INT_G_SDQ =~ prod1+...+prod18
    INT_AX_SDQ =~ prod15+prod16+prod17+prod18
    
    # direct effect
    L10 ~ a*L1 + b*L5 + c*INT_G_SDQ + d*INT_AX_SDQ
    
  # mediator
    NE ~ e*L1 + f*L5 + g*INT_G_SDQ + h*INT_AX_SDQ
    L10 ~ i*NE
    
    # indirect effect (a*b)
    ei := e*i
    fi := f*i
    gi := g*i
    hi := h*i
    
    # total effect
    total1 := a + (e*i)
    total2 := b + (f*i)
    total3 := c + (g*i)
    total4 := d + (h*i)
"

Model_SDQ <- lavaan::sem(fit25_SDQ, data = m.data,
                    estimator = "MLR",  # ML for maximum likelihood, ... 
                    missing = "fiml",   # "ml" full information maximum likelihood approach (fiml); "listwise",
                    std.ov = TRUE,  # TRUE:all observed variables are standardized before entering the analysis
                    std.lv = TRUE,  # TRUE:the metric of each latent is determined by fixing residual variance to 1
                    orthogonal = T,  # TRUE:all covariances among latents are set to 0
                    meanstructure = TRUE)

summary(Model_SDQ, standardized = T, fit.measures = T)
standardizedsolution(Model_SDQ)
```

### (2) MDD model

``` r
fit25_MDD <- "
    L1 =~ Pren_CESD1 + ... + Pren_pregn_anx_q4

  ...

    INT_G_MDD =~ prod1+...+prod18
    INT_AX_MDD =~ prod15+...+prod18
    
    # direct effect
    L10 ~ a*L1 + b*L5 + c*INT_G_MDD + d*INT_AX_MDD
    
    # mediator
    NE ~ e*L1 + f*L5 + g*INT_G_MDD + h*INT_AX_MDD
    L10 ~ i*NE

    # indirect effect (a*b)
    ei := e*i
    fi := f*i
    gi := g*i
    hi := h*i
    
    # total effect
    total1 := a + (e*i)
    total2 := b + (f*i)
    total3 := c + (g*i)
    total4 := d + (h*i)
"
```

------------------------------------------------------------------------

3. Results
----------

-   Computation time: ~5 min

### - Model Summary

#### (1) SDQ Model

``` r
lavaan 0.6-5.1458 ended normally after 101 iterations

  Estimator                                         ML               
  Optimization method                           NLMINB               
  Number of free parameters                        245               
                                                                     
  Number of observations                           639
  Number of missing patterns                       158

  Model Fit Test Statistic                    3353.084    3117.127
  Degrees of freedom                              1834        1834
  P-value (Chi-square)                           0.000       0.000
  Scaling correction factor                                  1.076
    for the Yuan-Bentler correction (Mplus variant)

Model test baseline model:

  Minimum Function Test Statistic            17375.204   15245.178
  Degrees of freedom                              1953        1953
  P-value                                        0.000       0.000
  Scaling correction factor                                  1.140

User model versus baseline model:

  Comparative Fit Index (CFI)                    0.902       0.903
  Tucker-Lewis Index (TLI)                       0.895       0.897

  Robust Comparative Fit Index (CFI)                         0.909
  Robust Tucker-Lewis Index (TLI)                            0.903

Loglikelihood and Information Criteria:

  Loglikelihood user model (H0)             -24833.844  -24833.844
  Scaling correction factor                                  1.794
    for the MLR correction
  Loglikelihood unrestricted model (H1)     -23157.302  -23157.302
  Scaling correction factor                                  1.160
    for the MLR correction

  Number of free parameters                        245         245
  Akaike (AIC)                               50157.687   50157.687
  Bayesian (BIC)                             51250.364   51250.364
  Sample-size adjusted Bayesian (BIC)        50472.506   50472.506

Root Mean Square Error of Approximation:

  RMSEA                                          0.036       0.033
  90 Percent Confidence Interval          0.034  0.038       0.031  0.035
  P-value RMSEA <= 0.05                          1.000       1.000

  Robust RMSEA                                               0.034
  90 Percent Confidence Interval                             0.032  0.036

Standardized Root Mean Square Residual:

  SRMR                                           0.065       0.065

Parameter Estimates:

  Information                                 Observed
  Observed information based on                Hessian
  Standard Errors                   Robust.huber.white
```

#### (2) MDD Model

``` r
lavaan 0.6-5.1458 ended normally after 99 iterations

  Estimator                                         ML               
  Optimization method                           NLMINB               
  Number of free parameters                        245               
                                                                     
  Number of observations                           639
  Number of missing patterns                       158

  Model Fit Test Statistic                    3495.596    3268.130
  Degrees of freedom                              1834        1834
  P-value (Chi-square)                           0.000       0.000
  Scaling correction factor                                  1.070
    for the Yuan-Bentler correction (Mplus variant)

Model test baseline model:

  Minimum Function Test Statistic            17177.555   14857.731
  Degrees of freedom                              1953        1953
  P-value                                        0.000       0.000
  Scaling correction factor                                  1.156

User model versus baseline model:

  Comparative Fit Index (CFI)                    0.891       0.889
  Tucker-Lewis Index (TLI)                       0.884       0.882

  Robust Comparative Fit Index (CFI)                         0.897
  Robust Tucker-Lewis Index (TLI)                            0.891

Loglikelihood and Information Criteria:

  Loglikelihood user model (H0)             -25003.924  -25003.924
  Scaling correction factor                                  2.055
    for the MLR correction
  Loglikelihood unrestricted model (H1)     -23256.126  -23256.126
  Scaling correction factor                                  1.186
    for the MLR correction

  Number of free parameters                        245         245
  Akaike (AIC)                               50497.848   50497.848
  Bayesian (BIC)                             51590.524   51590.524
  Sample-size adjusted Bayesian (BIC)        50812.667   50812.667

Root Mean Square Error of Approximation:

  RMSEA                                          0.038       0.035
  90 Percent Confidence Interval          0.036  0.040       0.033  0.037
  P-value RMSEA <= 0.05                          1.000       1.000

  Robust RMSEA                                               0.036
  90 Percent Confidence Interval                             0.034  0.038

Standardized Root Mean Square Residual:

  SRMR                                           0.070       0.070

Parameter Estimates:

  Information                                 Observed
  Observed information based on                Hessian
  Standard Errors                   Robust.huber.white
```

### - Coefficients

#### (1) SDQ Model

``` r
Regressions:
                   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
  L10 ~                                                                 
    L1         (a)    0.215    0.090    2.376    0.018    0.176    0.176
    L5         (b)    0.077    0.081    0.942    0.346    0.063    0.063
    INT_G_SDQ  (c)    0.219    0.095    2.298    0.022    0.180    0.180
    INT_AX_SDQ (d)    0.011    0.111    0.099    0.921    0.009    0.009
  NE ~                                                                  
    L1         (e)    0.359    0.074    4.868    0.000    0.333    0.333
    L5         (f)    0.171    0.090    1.909    0.056    0.158    0.158
    INT_G_SDQ  (g)   -0.033    0.084   -0.393    0.694   -0.030   -0.030
    INT_AX_SDQ (h)    0.083    0.093    0.890    0.374    0.077    0.077
  L10 ~                                                                 
    NE         (i)    0.504    0.093    5.418    0.000    0.448    0.448

...

Defined Parameters:
                   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ei                0.181    0.053    3.425    0.001    0.149    0.149
    fi                0.086    0.048    1.784    0.074    0.071    0.071
    gi               -0.017    0.042   -0.394    0.693   -0.014   -0.014
    hi                0.042    0.046    0.902    0.367    0.034    0.034
    total1            0.396    0.089    4.430    0.000    0.325    0.325
    total2            0.163    0.089    1.827    0.068    0.134    0.134
    total3            0.202    0.107    1.894    0.058    0.166    0.166
    total4            0.053    0.110    0.478    0.633    0.043    0.043

...

Defined Parameters:
  
    L10 ~ a*L1 + b*L5 + c*INT_G_SDQ + d*INT_AX_SDQ
    
  # mediator
    NE ~ e*L1 + f*L5 + g*INT_G_SDQ + h*INT_AX_SDQ
    L10 ~ i*NE
    
    # indirect effect (a*b)
    ei := e*i
    fi := f*i
    gi := g*i
    hi := h*i
    
    # total effect
    total1 := a + (e*i)
    total2 := b + (f*i)
    total3 := c + (g*i)
    total4 := d + (h*i)
```

#### (2) MDD Model

``` r
Regressions:
                   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
  L10 ~                                                                 
    L1         (a)    0.205    0.096    2.129    0.033    0.170    0.170
    L5         (b)    0.088    0.080    1.095    0.273    0.073    0.073
    INT_G_MDD  (c)    0.055    0.106    0.517    0.605    0.045    0.045
    INT_AX_MDD (d)    0.048    0.106    0.454    0.650    0.040    0.040
  NE ~                                                                  
    L1         (e)    0.378    0.077    4.900    0.000    0.340    0.340
    L5         (f)    0.187    0.094    2.000    0.046    0.168    0.168
    INT_G_MDD  (g)    0.151    0.084    1.802    0.072    0.135    0.135
    INT_AX_MDD (h)   -0.194    0.120   -1.614    0.106   -0.175   -0.175
  L10 ~                                                                 
    NE         (i)    0.503    0.095    5.285    0.000    0.463    0.463

...

Defined Parameters:
                   Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ei                0.190    0.054    3.558    0.000    0.157    0.157
    fi                0.094    0.049    1.913    0.056    0.078    0.078
    gi                0.076    0.043    1.776    0.076    0.063    0.063
    hi               -0.098    0.060   -1.639    0.101   -0.081   -0.081
    total1            0.395    0.093    4.254    0.000    0.327    0.327
    total2            0.182    0.084    2.169    0.030    0.151    0.151
    total3            0.131    0.109    1.204    0.228    0.108    0.108
    total4           -0.050    0.102   -0.489    0.625   -0.041   -0.041

...

Defined Parameters:
  
    L10 ~ a*L1 + b*L5 + c*INT_G_SDQ + d*INT_AX_SDQ
    
  # mediator
    NE ~ e*L1 + f*L5 + g*INT_G_SDQ + h*INT_AX_SDQ
    L10 ~ i*NE
    
    # indirect effect (a*b)
    ei := e*i
    fi := f*i
    gi := g*i
    hi := h*i
    
    # total effect
    total1 := a + (e*i)
    total2 := b + (f*i)
    total3 := c + (g*i)
    total4 := d + (h*i)
```

------------------------------------------------------------------------
