RIFLE 3 Logistic in Global 
========================================================

Procalcitonin and CHF interaction:



```r
anonmaster5 <- read.csv("~/Desktop/Procalcitonin Day 01 Priject/anonmaster5")
library(rms)
```

```
## Loading required package: Hmisc
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: splines
## Loading required package: Formula
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
## 
## Loading required package: SparseM
## 
## Attaching package: 'SparseM'
## 
## The following object is masked from 'package:base':
## 
##     backsolve
```

```r
# anonmaster5 <- subset(anonmaster4, ESRD_CKD5=="non ESRD.CKD5")
fit <- lrm(data=anonmaster5, rifle3 ~ CR.d01 + PROCALCITONIN.d01 +CHF+PROCALCITONIN.d01*CHF, x=TRUE,y=TRUE)
fit
```

```
## 
## Logistic Regression Model
## 
## lrm(formula = rifle3 ~ CR.d01 + PROCALCITONIN.d01 + CHF + PROCALCITONIN.d01 * 
##     CHF, data = anonmaster5, x = TRUE, y = TRUE)
## 
## Frequencies of Missing Values Due to Each Variable
##            rifle3            CR.d01 PROCALCITONIN.d01               CHF 
##                 3                 3                 0                 0 
## 
## 
##                       Model Likelihood     Discrimination    Rank Discrim.    
##                          Ratio Test            Indexes          Indexes       
## Obs           600    LR chi2      50.04    R2       0.210    C       0.750    
##  FALSE        561    d.f.             4    g        0.869    Dxy     0.499    
##  TRUE          39    Pr(> chi2) <0.0001    gr       2.384    gamma   0.508    
## max |deriv| 8e-08                          gp       0.062    tau-a   0.061    
##                                            Brier    0.051                     
## 
##                                 Coef    S.E.   Wald Z Pr(>|Z|)
## Intercept                       -3.8590 0.3925 -9.83  <0.0001 
## CR.d01                           0.7044 0.1326  5.31  <0.0001 
## PROCALCITONIN.d01                0.0220 0.0093  2.36  0.0182  
## CHF=non CHF                     -0.3585 0.3968 -0.90  0.3663  
## PROCALCITONIN.d01 * CHF=non CHF -0.0308 0.0158 -1.95  0.0515
```

```r
with(anonmaster5,table(rifle3))
```

```
## rifle3
## FALSE  TRUE 
##   561    39
```
