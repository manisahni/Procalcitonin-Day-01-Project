## Risk Models for AKI, ARF and RRT in CHF

========================================================





```r
chf <- read.csv("~/Desktop/Procalcitonin Day 01 Priject/chf.csv")
```

## Development of RIFLE grade 1 AKI in Hospitalised CHF Patients.


```r
chf.aki1<- glm(devaki~log(PROCALCITONIN.d01)+LACTATE.d01+WBC.d01+Htn+DM2+PHtn+min_sodium+CR.d01+gender+ALB.d01+AGE+BUN.Cr+HCO3,data=chf, family=binomial(link=logit))

summary(chf.aki1)
```

```
## 
## Call:
## glm(formula = devaki ~ log(PROCALCITONIN.d01) + LACTATE.d01 + 
##     WBC.d01 + Htn + DM2 + PHtn + min_sodium + CR.d01 + gender + 
##     ALB.d01 + AGE + BUN.Cr + HCO3, family = binomial(link = logit), 
##     data = chf)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.1034  -0.3965  -0.1754  -0.0891   2.7484  
## 
## Coefficients:
##                        Estimate Std. Error z value Pr(>|z|)  
## (Intercept)              9.2409    14.0006    0.66     0.51  
## log(PROCALCITONIN.d01)   0.7174     0.3499    2.05     0.04 *
## LACTATE.d01             -0.0832     0.1802   -0.46     0.64  
## WBC.d01                 -0.0460     0.0681   -0.67     0.50  
## Htnno htn               -0.8175     0.9884   -0.83     0.41  
## DM2no DM2               -1.1363     1.1970   -0.95     0.34  
## PHtnPHtn                 0.7248     1.6297    0.44     0.66  
## min_sodium              -0.1013     0.1138   -0.89     0.37  
## CR.d01                  -1.0942     0.9298   -1.18     0.24  
## genderM                  0.3248     1.2845    0.25     0.80  
## ALB.d01                  0.9611     0.8779    1.09     0.27  
## AGE                     -0.0131     0.0324   -0.40     0.69  
## BUN.Cr                   0.0247     0.0587    0.42     0.67  
## HCO3                     0.0354     0.1145    0.31     0.76  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 55.417  on 97  degrees of freedom
## Residual deviance: 40.904  on 84  degrees of freedom
##   (109 observations deleted due to missingness)
## AIC: 68.9
## 
## Number of Fisher Scoring iterations: 7
```

```r
int <- exp(confint(chf.aki1))
```

```
## Waiting for profiling to be done...
```

```r
odds <- exp(coef(chf.aki1))
aki1oddstable <- cbind(odds,int)
aki1oddstable
```

```
##                             odds     2.5 %    97.5 %
## (Intercept)            1.031e+04 1.489e-08 5.116e+16
## log(PROCALCITONIN.d01) 2.049e+00 1.128e+00 4.713e+00
## LACTATE.d01            9.202e-01 5.909e-01 1.265e+00
## WBC.d01                9.551e-01 8.234e-01 1.075e+00
## Htnno htn              4.415e-01 5.463e-02 2.940e+00
## DM2no DM2              3.210e-01 2.104e-02 2.820e+00
## PHtnPHtn               2.064e+00 8.302e-02 5.720e+01
## min_sodium             9.036e-01 7.113e-01 1.122e+00
## CR.d01                 3.348e-01 3.795e-02 1.453e+00
## genderM                1.384e+00 1.166e-01 2.111e+01
## ALB.d01                2.614e+00 5.251e-01 1.786e+01
## AGE                    9.870e-01 9.228e-01 1.054e+00
## BUN.Cr                 1.025e+00 9.119e-01 1.159e+00
## HCO3                   1.036e+00 8.180e-01 1.301e+00
```

```r
stargazer(aki1oddstable)
```

```
## Error: could not find function "stargazer"
```

## Model for Development of RIFLE 3  AKF in Hospitalised CHF Patients.

Criterion for RIFLE 3 AKF as follows:

devakirifle3=(d2max_cr/CR.d01)>=3)
rifle4 = (CR.d01 >=4 & ((d2max_cr-CR.d01)>=0.5)))
rifle3 = (devakirifle3=="TRUE" | rrtbin =="TRUE" | rifle4 == "TRUE"))



```r
## glm for rifle3
chfrifle3 <- glm(rifle3~log(PROCALCITONIN.d01)+LACTATE.d01+ WBC.d01+DM2+CR.d01+gender+ALB.d01+PHtn+Htn+AGE+min_sodium+min_hb+BUN.Cr+HCO3,data=chf, family=binomial(link=logit))

summary(chfrifle3)
```

```
## 
## Call:
## glm(formula = rifle3 ~ log(PROCALCITONIN.d01) + LACTATE.d01 + 
##     WBC.d01 + DM2 + CR.d01 + gender + ALB.d01 + PHtn + Htn + 
##     AGE + min_sodium + min_hb + BUN.Cr + HCO3, family = binomial(link = logit), 
##     data = chf)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.3963  -0.4071  -0.1334  -0.0281   2.4705  
## 
## Coefficients:
##                        Estimate Std. Error z value Pr(>|z|)  
## (Intercept)             13.8380    13.7120    1.01    0.313  
## log(PROCALCITONIN.d01)   0.9737     0.4088    2.38    0.017 *
## LACTATE.d01              0.1088     0.1299    0.84    0.402  
## WBC.d01                 -0.0330     0.0532   -0.62    0.535  
## DM2no DM2                0.3356     0.8996    0.37    0.709  
## CR.d01                   1.1788     0.5751    2.05    0.040 *
## genderM                  1.4691     1.1602    1.27    0.205  
## ALB.d01                  0.1654     0.8019    0.21    0.837  
## PHtnPHtn                 2.6217     1.3381    1.96    0.050 .
## Htnno htn               -1.6303     1.0001   -1.63    0.103  
## AGE                     -0.0334     0.0273   -1.22    0.222  
## min_sodium              -0.1672     0.1089   -1.54    0.125  
## min_hb                   0.1650     0.1987    0.83    0.406  
## BUN.Cr                   0.0923     0.0659    1.40    0.161  
## HCO3                    -0.0107     0.1299   -0.08    0.934  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 72.868  on 97  degrees of freedom
## Residual deviance: 44.129  on 83  degrees of freedom
##   (109 observations deleted due to missingness)
## AIC: 74.13
## 
## Number of Fisher Scoring iterations: 7
```

```r
##odds ratio table
int <- exp(confint(chfrifle3))
```

```
## Waiting for profiling to be done...
```

```r
odds <- exp(coef(chfrifle3))
akioddstable <- cbind(odds,int)
akioddstable
```

```
##                             odds     2.5 %    97.5 %
## (Intercept)            1.023e+06 0.0000138 3.695e+18
## log(PROCALCITONIN.d01) 2.648e+00 1.3358128 6.891e+00
## LACTATE.d01            1.115e+00 0.8568587 1.454e+00
## WBC.d01                9.675e-01 0.8547723 1.061e+00
## DM2no DM2              1.399e+00 0.2200248 8.527e+00
## CR.d01                 3.250e+00 1.3897892 1.133e+01
## genderM                4.345e+00 0.4975411 5.241e+01
## ALB.d01                1.180e+00 0.2514035 6.512e+00
## PHtnPHtn               1.376e+01 1.1441605 2.546e+02
## Htnno htn              1.959e-01 0.0226610 1.252e+00
## AGE                    9.672e-01 0.9133568 1.020e+00
## min_sodium             8.460e-01 0.6631888 1.018e+00
## min_hb                 1.179e+00 0.8073966 1.814e+00
## BUN.Cr                 1.097e+00 0.9684002 1.264e+00
## HCO3                   9.893e-01 0.7578113 1.276e+00
```

## Model for Development of RIFLE 2  AKF in Hospitalised CHF Patients.

(d2maxcr/Cr.d01 >2)

*Here we subset the anonMASTER file and define the outcome of RIFLE 2 renal injury. We subset the data for CHF patients and generate and odds ratio table.*


```r
chfrifle2 <- glm(devakirifle2~log(PROCALCITONIN.d01)+LACTATE.d01+ WBC.d01+DM2+CR.d01+gender+ALB.d01+AGE+PHtn+Htn+ESLD+BUN.Cr+HCO3,data=chf, family=binomial(link=logit))
summary(chfrifle2)
```

```
## 
## Call:
## glm(formula = devakirifle2 ~ log(PROCALCITONIN.d01) + LACTATE.d01 + 
##     WBC.d01 + DM2 + CR.d01 + gender + ALB.d01 + AGE + PHtn + 
##     Htn + ESLD + BUN.Cr + HCO3, family = binomial(link = logit), 
##     data = chf)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.3556  -0.0640  -0.0089  -0.0003   2.0748  
## 
## Coefficients:
##                         Estimate Std. Error z value Pr(>|z|)  
## (Intercept)              -1.7583  2841.6311    0.00    1.000  
## log(PROCALCITONIN.d01)    2.0641     0.9682    2.13    0.033 *
## LACTATE.d01              -0.7005     0.5844   -1.20    0.231  
## WBC.d01                  -0.0891     0.0962   -0.93    0.355  
## DM2no DM2                -6.7485     3.6087   -1.87    0.061 .
## CR.d01                   -4.1636     2.4819   -1.68    0.093 .
## genderM                   0.3133     2.6666    0.12    0.906  
## ALB.d01                  -0.5818     1.3383   -0.43    0.664  
## AGE                      -0.1451     0.0794   -1.83    0.068 .
## PHtnPHtn                 -0.7100     3.0118   -0.24    0.814  
## Htnno htn                -1.5823     1.9181   -0.82    0.409  
## ESLDnon ESLD             17.0303  2841.6092    0.01    0.995  
## BUN.Cr                    0.1441     0.1050    1.37    0.170  
## HCO3                     -0.0888     0.2655   -0.33    0.738  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 46.571  on 109  degrees of freedom
## Residual deviance: 21.934  on  96  degrees of freedom
##   (97 observations deleted due to missingness)
## AIC: 49.93
## 
## Number of Fisher Scoring iterations: 18
```

```r
int <- exp(confint(chfrifle2))
```

```
## Waiting for profiling to be done...
```

```r
odds <- exp(coef(chfrifle2))
akioddstable <- cbind(odds,int)
akioddstable
```

```
##                             odds     2.5 %     97.5 %
## (Intercept)            1.723e-01        NA 3.695e+105
## log(PROCALCITONIN.d01) 7.878e+00 2.002e+00  1.008e+02
## LACTATE.d01            4.963e-01 8.633e-02  1.041e+00
## WBC.d01                9.148e-01 7.293e-01  1.071e+00
## DM2no DM2              1.173e-03 4.515e-08  1.607e-01
## CR.d01                 1.555e-02 1.643e-05  4.812e-01
## genderM                1.368e+00 1.025e-02  8.738e+02
## ALB.d01                5.589e-01 2.130e-02  5.351e+00
## AGE                    8.650e-01 6.981e-01  9.719e-01
## PHtnPHtn               4.916e-01 5.461e-04  4.818e+02
## Htnno htn              2.055e-01 1.307e-03  5.131e+00
## ESLDnon ESLD           2.490e+07 1.163e-99         NA
## BUN.Cr                 1.155e+00 9.621e-01  1.504e+00
## HCO3                   9.151e-01 4.378e-01  1.512e+00
```
